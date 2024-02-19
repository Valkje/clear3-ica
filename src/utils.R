# Loads key presses and sessions, filters out subjects with <30 days of data
load_subject_key_data <- function(dat_dir, load_kp = TRUE, min_days = 30) {
  dirs <- list.dirs(dat_dir)

  pattern <- "sub-([0-9]+)/preproc$"
  preproc_paths <- str_subset(dirs, pattern)
  subjects <- str_match(preproc_paths, pattern)[,2]

  # Vector of indices for all eligible subjects
  sub_idx <- 1:length(subjects)

  # Loading dats_kp can take a while over Wi-Fi, so that's why it's optional
  dats_kp <- NULL
  if (load_kp) {
    load(file.path(dat_dir, "dats_kp.rda"))
  }

  load(file.path(dat_dir, "dats_ses.rda"))

  for (i in sub_idx) {
    if (n_distinct(dats_ses[[i]]$date) < min_days) {
      # Remove subject index
      sub_idx <- sub_idx[sub_idx != i]
    }
  }

  subjects <- subjects[sub_idx]
  dats_kp <- dats_kp[sub_idx]
  dats_ses <- dats_ses[sub_idx]

  list(subjects = subjects, dats_kp = dats_kp, dats_ses = dats_ses)
}

# Load and create summary measures of the TDIAs
summarize_tdias <- function(file, n_parts, permuted = FALSE, julia = FALSE) {
  if (julia) {
    ls <- parse_tdia_jld(file)
    tdias <- ls$tdias
    sampled_idx <- ls$sampled_idx
  } else {
    # Loads a list, inaptly named `tdias`
    load(file)

    sampled_idx <- tdias[2,]
    # List [10000] of n_sub*n_parts matrix
    tdias <- tdias[1,]
    # [n_sub*n_parts]*10000, where every n_parts rows belong to one subject
    tdias <- sapply(tdias, function(x) as.vector(t(x)))
  }

  iter_means <- data.frame(
    iter_mean = colMeans(tdias),
    permuted = permuted
  )

  # Mostly used for later summary calculations
  tdia_df <- data.frame(
    subject = rep(1:(dim(tdias)[1]/n_parts), each = n_parts),
    tdias
  )

  # Mean variance within subjects, between partitions, across iterations
  sub_var <- tdia_df %>%
    group_by(subject) %>%
    summarize(
      across(X1:X10000, var) # column-wise
    ) %>%
    rowwise() %>%
    summarize(
      variance = mean(c_across(X1:X10000))
    ) %>%
    mutate(subject = 1:n())

  # Mean TDIA within subjects, first across partitions, then iterations
  sub_tdia <- tdia_df %>%
    group_by(subject) %>%
    summarize(
      across(X1:X10000, mean) # column-wise
    ) %>%
    rowwise() %>%
    mutate(
      average = mean(c_across(X1:X10000))
    ) %>%
    relocate(average, .before = X1)

  # To be joined to tdia_df
  n_iter <- length(sampled_idx)
  n_sub <- length(sampled_idx[[1]])
  n_parts <- length(sampled_idx[[1]][[1]])
  idx_df <- data.frame(
    iteration = rep(1:n_iter, each = n_sub*n_parts),
    subject = rep(1:n_sub, each = n_parts, times = n_iter),
    partition_order = rep(1:n_parts, times = n_iter*n_sub),
    partition_idx = unlist(sampled_idx)
  )

  # Get TDIAs per original partition index
  partition_df <- tdia_df %>%
    pivot_longer(starts_with("X"),
                 names_to = "iteration",
                 names_prefix = "X",
                 values_to = "accuracy") %>%
    mutate(
      iteration = as.integer(iteration),
      partition_order = rep(1:n_parts, each = n_iter, times = n_sub)
    ) %>%
    arrange(iteration) %>%
    inner_join(idx_df, by = c("iteration", "subject", "partition_order")) %>%
    relocate(iteration, .before = subject) %>%
    relocate(accuracy, .after = partition_idx) %>%
    group_by(subject, partition_idx) %>%
    mutate(mean_accuracy = mean(accuracy)) %>%
    ungroup()

  list(
    tdias = tdias,
    sampled_idx = sampled_idx,
    iter_means = iter_means,
    sub_var = sub_var,
    sub_tdia = sub_tdia,
    partition_df = partition_df
  )
}

# Prints and returns some plots of the summary measures of a TDIA data set
plot_tdia_summ <- function(
    summ,
    sub = NULL,
    subjects = NULL,
    print_plots = TRUE
) {
  gs <- vector("list", length = 3)

  gs[[1]] <- ggplot(summ$iter_means, aes(iter_mean)) +
    geom_histogram() +
    xlab("Mean TDIA") +
    ylab("Count")

  gs[[2]] <- ggplot(melt(as.data.frame(summ$sub_tdia),
                         id.vars = c("subject", "average"),
                         variable_name = "partition"),
                    aes(subject)) +
    geom_point(aes(y = value), color = alpha("coral1", alpha = 0.01)) +
    geom_point(aes(y = average), color = "steelblue", size = 3) +
    xlab("subject") +
    ylab("Mean TDIA") +
    ylim(0, 1) +
    theme(text = element_text(size=15))

  if (!is.null(sub)) {
    if (is.null(subjects)) {
      stop("`subjects` must not be NULL when `sub` is not NULL")
    }

    sub_idx <- which(subjects == sub)

    gs[[3]] <- ggplot(summ$partition_df %>%
                        filter(subject == sub_idx),
                      aes(partition_idx)) +
      geom_point(aes(y = accuracy), color = "coral1", alpha = 0.01) +
      geom_point(aes(y = mean_accuracy), color = "steelblue", size = 3) +
      geom_line(aes(y = mean_accuracy), color = "steelblue") +
      scale_x_continuous("Partition index") +
      scale_y_continuous("TDIA", limits = c(0, 1)) +
      ggtitle(str_glue("Subject {sub}")) +
      theme_ipsum(base_size = 15, axis_title_size = 15)
  }

  if (print_plots) {
    lapply(gs, print)
  }

  gs
}

# Compares an unpermuted TDIA set with a permuted one, by plotting their
# histograms in one plot
plot_tdia_comp <- function(summ, summ_perm, key = NULL, legend_title = "Permuted") {
  summ$iter_means$key <- key[1:nrow(summ$iter_means)]
  summ_perm$iter_means$key <- key[(nrow(summ$iter_means)+1):(2*nrow(summ_perm$iter_means))]

  iter_all_means <- rbind(summ$iter_means, summ_perm$iter_means)

  fill <- "permuted"

  if (!is.null(key)) {
    fill <- "key"
  }

  g <- ggplot(iter_all_means, aes(iter_mean, fill = .data[[fill]])) +
    geom_histogram(data = summ$iter_means,
                   color="#e9ecef", bins = 100, alpha = 0.5) +
    geom_histogram(data = summ_perm$iter_means,
                   color="#e9ecef", bins = 100, alpha = 0.5) +
    xlab("Mean TDIA") +
    ylab("Count") +
    guides(fill = guide_legend(legend_title, reverse = T)) +
    scale_fill_ipsum() +
    theme_ipsum(base_size = 15, axis_title_size = 15)

  print(g)

  g
}

# Parse Julia's JLD TDIA files
parse_tdia_jld <- function(path) {
  h5f <- H5Fopen(path)

  # n_sub X n_parts X n_iter
  tdias <- h5f$tdias

  # n_parts X n_sub X n_iter
  tdias <- aperm(tdias, c(2, 1, 3))

  ds <- dim(tdias)
  n_parts <- ds[1]
  n_sub <- ds[2]
  n_iter <- ds[3]

  # [n_sub*n_parts] X n_iter, where the first n_parts rows are from the same
  # subject
  dim(tdias) <- c(ds[1] * ds[2], ds[3])

  # [n_sub*n_iter] X n_parts, where the first n_iter rows are from the same
  # subject
  sampled_idx <- h5f$sampled_idx

  # Convert sampled_idx to a list of lists of vectors (n_iter X n_sub X n_parts)
  iter_labels <- rep(1:n_iter, times = n_sub * n_parts)
  sampled_idx <- split(sampled_idx, iter_labels)

  sub_labels <- rep(1:n_sub, times = n_parts)
  sampled_idx <- lapply(sampled_idx, split, sub_labels)

  # n_sub X n_sub
  conf_mat <- drop(h5f$conf_mat)

  H5Fclose(h5f)

  list(tdias = tdias, sampled_idx = sampled_idx, conf_mat = conf_mat)
}

# We have to:
#
# * Compute the average TDIA per subject and per partition day. For that, we
#   first need to relate all `n_parts` partitions of a subject in an iteration
#   to that subject's day index.
# * Relate every partition back to a date.
link_tdia_date <- function(tdias, sampled_idx, subjects, dats_kp, weekly) {

  ## Label TDIA data

  # Every column represents an iteration, every row a (sampled) partition of a
  # subject
  n_parts <- length(sampled_idx[[1]][[1]])
  tdia_df <- data.frame(
    subject = rep(1:(dim(tdias)[1]/n_parts), each = n_parts),
    tdias
  )

  # unlist gets you a vector where every n_parts entries are from one subject,
  # and every n_sub*n_parts entries are from one iteration.

  n_iter <- length(sampled_idx)
  n_sub <- length(sampled_idx[[1]])
  idx_df <- data.frame(
    iteration = rep(1:n_iter, each = n_sub*n_parts),
    subject = rep(1:n_sub, each = n_parts, times = n_iter),
    partition_order = rep(1:n_parts, times = n_iter*n_sub),
    partition_idx = unlist(sampled_idx)
  )

  ## Join TDIAs with partition indices.

  partition_df <- tdia_df %>%
    pivot_longer(starts_with("X"),
                 names_to = "iteration",
                 names_prefix = "X",
                 values_to = "accuracy") %>%
    mutate(
      iteration = as.integer(iteration),
      partition_order = rep(1:n_parts, each = n_iter, times = n_sub)
    ) %>%
    arrange(iteration) %>%
    inner_join(idx_df, by = c("iteration", "subject", "partition_order")) %>%
    relocate(iteration, .before = subject) %>%
    relocate(accuracy, .after = partition_idx) %>%
    group_by(subject, partition_idx) %>%
    summarize(mean_accuracy = mean(accuracy)) %>%
    ungroup() %>%
    mutate(subject = as.integer(subjects[subject]))


  ## Relate partition days back to dates

  # Create a data frame with subject index and date for which (alphanumeric-to-
  # alphanumeric) key presses have been recorded.

  kp_df <- bind_rows(dats_kp, .id = "subject_idx")

  sub_dates <- kp_df %>%
    group_by(subject_idx) %>%
    mutate(
      subject_idx = as.integer(subject_idx),
      subject = as.integer(subjects[subject_idx]),
      date = date(keypressTimestampLocal),
      week = week(keypressTimestampLocal)
    ) %>%
    ungroup() %>%
    filter(keypress_type == "alphanum", previousKeyType == "alphanum",
           subject == lag(subject))

  # Could be done in a more compact way, but I can't be bothered
  if (weekly) {
    sub_dates <- sub_dates %>%
      select(subject, week)
  } else {
    sub_dates <- sub_dates %>%
      select(subject, date)
  }

  sub_dates <- sub_dates %>%
    distinct() %>%
    group_by(subject) %>%
    mutate(partition_idx = 1:n()) %>%
    ungroup()

  ## Join the dates and TDIAs on subject

  sub_tdia <- partition_df %>%
    inner_join(sub_dates, by = c("subject", "partition_idx")) %>%
    mutate(subject = factor(subject))

  sub_tdia
}

estimates_to_df <- function(
  models,
  include_corrected = FALSE,
  transpose = FALSE,
  correctionFactor = NULL
) {
  terms <- c("medianIKD", "percent95IKD", "madIKD", "autocorrectRate",
             "backspaceRate", "totalKeyPresses", "active", "upright")
  
  labels <- c(
    medianIKD = "Median IKD",
    percent95IKD = "95th percentile IKD",
    madIKD = "MAD IKD",
    autocorrectrate = "Autocorrect rate",
    backspacerate = "Backspace rate",
    totalKeyPresses = "Total number of key presses",
    active = "Movement rate",
    upright = "Upright rate"
  )
  
  n_models <- length(models)
  n_terms <- length(terms)
  
  ests <- lapply(models, function(m) {
    summ <- summary(m[[1]])
    tab <- summ$tTable
    
    betas <- tab[terms, 1]
    p_vals <- tab[terms, 5]
    
    df <- data.frame(b = betas, p_prime = p_vals)
    
    resp_var <- attr(getResponse(m[[1]]), "label")
    
    col_names <- c(paste0(resp_var, ".beta"), paste0(resp_var, ".p'"))
    
    if (include_corrected) {
      if (is.null(correctionFactor))
        correctionFactor = n_models * n_terms
      
      df <- df %>%
        mutate(p = pmin(1, correctionFactor * p_prime))
      
      col_names <- c(col_names, paste0(resp_var, ".p"))
    }
    
    colnames(df) <- col_names
    rownames(df) <- labels
    
    df
  })
  
  df <- bind_cols(ests) %>%
    mutate(
      across(everything(), function(x) {
        x <- sprintf("%#.2g", x)
        x[x == "1.0"] <- "1"
        x
      })
    )
  
  if (transpose) {
    # Transpose, convert back to data frame, reverse columns
    df <- rev(data.frame(t(df), check.names = FALSE))
  }
  
  df
}

# Prints relevant model estimates to a neat csv
estimates_to_csv <- function(
  file, 
  models, 
  include_corrected = FALSE, 
  transpose = FALSE, 
  correctionFactor = NULL
) {
  df <- estimates_to_df(models, include_corrected, transpose, correctionFactor)

  write.csv(df, file)

  df
}
