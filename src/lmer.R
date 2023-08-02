prep_reg_dat <- function(dat_path, ica_path) {
  # Semi-contiguous or fragmented data
  dat_reg <- readRDS(dat_path)
  
  dat_bi <- dat_reg %>%
    filter(modality == "biaffect") %>%
    select(!c(study_start_date, treatment_start_date, modality)) %>%
    pivot_wider(names_from = variable, values_from = value) %>%
    select(!partition_idx)
  
  print(str_glue("Number of BiAffect cases: {nrow(dat_bi)}"))
  print(str_glue("\tNumber of subjects: {n_distinct(dat_bi$subject)}"))
  
  dat_sr <- dat_reg %>%
    filter(modality == "self_report") %>%
    select(!c(study_start_date, treatment_start_date, modality)) %>%
    pivot_wider(names_from = variable, values_from = value)
  
  print(str_glue("Number of self-report cases: {nrow(dat_sr)}"))
  print(str_glue("\tNumber of subjects: {n_distinct(dat_sr$subject)}"))
  
  # RDA file
  load(ica_path)
  ns_comp <- as.integer(names(icas))
  
  # Combine BiAffect data with self-report ICs
  dats <- lapply(icas, function(ica) {
    data.frame(ica$S) %>%
      add_column(subject = subs, date = dates) %>%
      inner_join(dat_bi, by = c("subject", "date")) %>%
      mutate(
        week = week(date),
        sub_week = paste(subject, week, sep = " - ") # For convenience
      ) %>%
      relocate(subject, date, week, sub_week, .before = X1)
  })
  
  # Trash weeks within subject with only one observation
  print(str_glue("Complete-case entries in combined data: {nrow(dats[[1]])}"))
  print(str_glue("\tNumber of subjects: {n_distinct(dats[[1]]$subject)}"))
  
  dats_f <- lapply(dats, function(dat) {
    dat %>%
      group_by(sub_week) %>%
      filter(n() > 1) %>%
      ungroup()
  })
  
  print(str_glue("Entries in data after filtering out weeks with one observation: {nrow(dats_f[[1]])}"))
  print(str_glue("\tNumber of subjects: {n_distinct(dats_f[[1]]$subject)}"))
  
  dats_c <- lapply(dats_f, function(dat) {
    dat %>%
      mutate(
        totalKeyPresses = log(totalKeyPresses),
        across(medianIKD:mean_accuracy, scale)
      )
  })
  
  list(
    dat_reg = dat_reg, 
    dat_bi = dat_bi, 
    dat_sr = dat_sr, 
    icas = icas,
    subs = subs,
    dates = dates,
    dats_c = dats_c
  )
}