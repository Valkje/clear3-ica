# Get x and y breaks of the 2D histogram of all data
calc_breaks <- function(dats_kp) {
  n_sub <- length(dats_kp)
  
  dists <- vector(mode = "list", n_sub)
  ikds <- vector(mode = "list", n_sub)
  
  for (i in 1:n_sub) {
    dat_kp <- dats_kp[[i]]
    
    dat_kp_alphanum <- dat_kp %>%
      filter(keypress_type == "alphanum", previousKeyType == "alphanum")
    
    dists[[i]] <- dat_kp_alphanum$distanceFromPrevious
    ikds[[i]] <- dat_kp_alphanum$IKD
  }
  
  ls <- bin2d(unlist(dists), unlist(ikds), xBins = 100, yBins = 100)
  
  list(xBreaks = ls$xBreaks, yBreaks = ls$yBreaks)
}

# Get breaks of the ND histogram of all data
calc_breaks_nd <- function(dats_kp, columns) {
  n_sub <- length(dats_kp)
  
  dats_kp_alphanum <- lapply(dats_kp, function (dat_kp) {
    filter(dat_kp, keypress_type == "alphanum", previousKeyType == "alphanum")
  })
  
  cols_lists <- lapply(columns, function(c) {
    lapply(dats_kp_alphanum, function(dat) dat[c])
  })
  
  mat <- sapply(cols_lists, unlist, TRUE, FALSE)
  
  ls <- binNd(mat, rep(10, length(columns)))
  
  ls$breaks
}

# Calculate histogram matrix per day or week
calc_partitions <- function(dats_kp, breaks) {
  n_sub <- length(dats_kp)
  
  # List of partition data frames for all subjects
  subject_partitions <- vector("list", n_sub)
  
  for (i in 1:n_sub) {
    dat_kp <- dats_kp[[i]]
    
    dat_kp_alphanum <- dat_kp %>%
      filter(keypress_type == "alphanum", previousKeyType == "alphanum")
    
    dat_kp_alphanum <- dat_kp_alphanum %>%
      mutate(
        date = date(keypressTimestampLocal),
        week = week(keypressTimestampLocal) # Warning: If subjects have more than a year of data, this grouping will fail
      )
    n_days <- n_distinct(dat_kp_alphanum$date)
    
    subject_partitions[[i]] <- dat_kp_alphanum %>%
      group_by({{ grouping }}) %>%
      nest() %>%
      mutate(
        numberOfKeyPresses = nrow(data[[1]]),
        hist = map(data, function(df) {
          binCounts(df$distanceFromPrevious, 
                    df$IKD, 
                    breaks$xBreaks, 
                    breaks$yBreaks)
        })
      )
  }
  
  subject_mats <- lapply(subject_partitions, function(df) df$hist)
  
  subject_mats
}

# Compound loading and calculating histograms
get_partitions <- function(dat_dir, grouping = sym("date")) {
  dat <- load_subject_key_data(dat_dir)
  breaks <- calc_breaks(dat$dats_kp)
  subject_mats <- calc_partitions(dat$dats_kp, breaks, {{ grouping }})
  
  list(subject_mats = subject_mats, breaks = breaks, dat = dat)
}

test1 <- function(dat, var) {
  dat %>%
    group_by({{ var }})
}

test2 <- function(dat, var) {
  test1(dat, {{ var }})
}