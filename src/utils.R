# We have to:
#   
# * Compute the average TDIA per subject and per partition day. For that, we 
#   first need to relate all `n_parts` partitions of a subject in an iteration 
#   to that subject's day index.
# * Relate every partition back to a date.

link_tdia_date <- function(tdias, subjects, dats_kp) {
  
  ## Unpack and label TDIA data
  
  sampled_idx <- tdias[2,]
  # List [10000] of n_sub*n_parts matrix
  tdias <- tdias[1,]
  # [n_sub*n_parts]*10000, where every n_parts rows belong to one subject
  tdias <- sapply(tdias, function(x) as.vector(t(x)))
  
  
  # Every column represents an iteration, every row a (sampled) partition of a 
  # subject
  n_parts <- 10
  tdia_df <- data.frame(
    subject = rep(1:(dim(tdias)[1]/n_parts), each = n_parts), 
    tdias
  )
  
  # unlist gets you a vector where every n_parts entries are from one subject,
  # and every n_sub*n_parts entries are from one iteration.
  
  n_iter <- length(sampled_idx)
  n_sub <- length(sampled_idx[[1]])
  n_parts <- length(sampled_idx[[1]][[1]])
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
      date = date(keypressTimestampLocal)
    ) %>%
    ungroup() %>%
    filter(keypress_type == "alphanum", previousKeyType == "alphanum", 
           subject == lag(subject)) %>%
    select(subject, date) %>%
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