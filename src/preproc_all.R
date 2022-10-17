library(tidyverse)

setwd("~/Documents/Autocorrelation")

source("src/preproc.R")

filter <- dplyr::filter

dat_dir <- "data"

### Listing available raw data files

dirs <- list.dirs(dat_dir)

pattern <- "sub-([0-9]+)/raw$"
raw_paths <- str_subset(dirs, pattern)
subjects <- str_match(raw_paths, pattern)[,2]

### Preprocessing for every subject that has both key press and accelerometer
### data

for (sub in subjects) {
  print(str_glue("Preprocessing subject {sub}"))
  
  raw_path <- str_subset(raw_paths, str_glue("sub-{sub}"))
  kp_path <- file.path(raw_path, str_glue("User_{sub}_keypressDataMetrics.csv"))
  acc_path <- file.path(raw_path, str_glue("User_{sub}_accelData.csv"))
  
  print("Reading data...")
  raw_kp <- read.csv(kp_path)
  raw_acc <- read.csv(acc_path)
  
  print("Preprocessing accelerometer and key press data...")
  dat_acc <- preproc_acc(raw_acc)
  
  dats <- preproc_kp(raw_kp, dat_acc)
  dat_kp <- dats$dat_kp
  dat_ses <- dats$dat_ses
  
  print("Calculating entropy measures...")
  dat_kp_alphanum <- dat_kp %>%
    filter(keypress_type == "alphanum", previousKeyType == "alphanum")
  
  dat_dist <- hist_distances(dat_kp)
  dat_dist
  
  print(str_glue("Saving files for subject {sub}."))
  
  out_path <- file.path(dat_dir, str_glue("sub-{sub}"), "preproc")
  dir.create(out_path, showWarnings = FALSE, recursive = TRUE)
  
  out_file <- file.path(out_path, str_glue("sub-{sub}_preprocessed.rda"))
  save(dat_acc, dat_kp, dat_ses, dat_dist, file = out_file)
}