trim_baseline <- function(dat_reg, date_path, save_path) {
  # Contains start dates of the study (i.e. enrollment) and the
  # treatment/placebo
  start_dates <- read_excel(date_path) %>%
    mutate(
      ID = as.character(ID),
      study_start_date = as_date(study_start_date),
      treatment_start_date = as_date(treatment_start_date)
    )
  
  dat_reg_trimmed <- dat_reg %>%
    left_join(start_dates, c("subject" = "ID")) %>%
    relocate(study_start_date, treatment_start_date, .after = date) %>%
    filter(date >= study_start_date, 
           is.na(treatment_start_date) | date < treatment_start_date) %>%
    ungroup()
  
  saveRDS(dat_reg_trimmed, save_path)
}
