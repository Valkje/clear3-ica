if (.Platform$OS.type == "unix") {
  setwd("~/Documents/Autocorrelation")
} else {
  setwd("M:/Documents/Autocorrelation")
}

source("src/load_dependencies.R")

# The completely unmodified PSV files, as received from Faraz
src_dir <- "../CLEAR/"
# Base directory for the data set we will create
dat_dir <- "data"

### Move all source files into a BIDS-like structure
src_kp_dir <- file.path(src_dir, "Clear lab participants keystroke dynamics data")
src_acc_dir <- file.path(src_dir, "Clear lab participants keyboard accelerometer kinematics data")

# First transfer the key press files, if they are non-empty

pat <- "\\+(3[0-9]+)"

kp_files <- list.files(src_kp_dir, full.names = TRUE)
kp_files <- str_subset(kp_files, pat)
subjects <- str_match(kp_files, pat)[,2]

acc_files <- list.files(src_acc_dir, full.names = TRUE)
acc_files <- str_subset(acc_files, pat)
subjects <- base::union(subjects, str_match(acc_files, pat)[,2])

headers = c("distanceFromCenter", "distanceFromPrevious")
intendend_n_pipes <- 14

for (i in 1:length(subjects)) {
  sub <- subjects[i]
  kp_path <- kp_files[i]
  acc_path <- acc_files[i]
  
  sub_dir <- str_glue("sub-{sub}")
  raw_path <- file.path(dat_dir, sub_dir, "raw")
  
  print(str_glue("Processing source key press files for subject {sub}"))
  
  # Many key press files are missing the last two column headers,
  # so we will add them here if that's the case
  
  # Rather large files, so only read first two lines to check
  lin <- read_lines(kp_path)
  
  # If the file is empty (except for the headers), continue to the next subject
  if (length(lin) != 1) {
    actual_n_pipes <- str_count(lin[1], "\\|")
    
    sel <- (length(headers) + 1 - (intendend_n_pipes - actual_n_pipes)):length(headers)
    new_labels <- paste(headers[sel], collapse = "|")
    
    if (new_labels != "") 
      lin[1] <- paste(lin[1], new_labels, sep = "|")
    
    dir.create(raw_path, showWarnings = FALSE, recursive = TRUE)
    
    sub_kp_file <- file.path(raw_path, str_glue("sub-{sub}_keystrokes.psv"))
    write_lines(lin, sub_kp_file)
  }
  
  print(str_glue("Processing source accelerometer files for subject {sub}"))
  
  # Rather large files, so only read first two lines to check
  lin <- read_lines(acc_path, n_max = 2)
  
  if (length(lin) != 1) {
    dir.create(raw_path, showWarnings = FALSE, recursive = TRUE)
    
    sub_acc_file <- file.path(raw_path, str_glue("sub-{sub}_accelerometer.psv"))
    file.copy(acc_path, sub_acc_file, overwrite = TRUE)
  }
}

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
  kp_path <- file.path(raw_path, str_glue("sub-{sub}_keystrokes.psv"))
  acc_path <- file.path(raw_path, str_glue("sub-{sub}_accelerometer.psv"))
  
  print("Reading data...")
  raw_kp <- read.table(kp_path, header = TRUE, sep = "|", strip.white = TRUE, fill = TRUE)
  raw_acc <- read.table(acc_path, header = TRUE, sep = "|", strip.white = TRUE, fill = TRUE)
  
  print("Preprocessing accelerometer and key press data...")
  dat_acc <- preproc_acc(raw_acc)
  
  dats <- preproc_kp(raw_kp, dat_acc)
  dat_kp <- dats$dat_kp
  dat_ses <- dats$dat_ses
  
  # print("Calculating entropy measures...")
  # dat_kp_alphanum <- dat_kp %>%
  #   filter(keypress_type == "alphanum", previousKeyType == "alphanum")
  # 
  # dat_dist <- hist_distances(dat_kp)
  # dat_dist
  
  print(str_glue("Saving files for subject {sub}."))
  
  out_path <- file.path(dat_dir, str_glue("sub-{sub}"), "preproc")
  dir.create(out_path, showWarnings = FALSE, recursive = TRUE)
  
  out_file <- file.path(out_path, str_glue("sub-{sub}_preprocessed.rda"))
  # save(dat_acc, dat_kp, dat_ses, dat_dist, file = out_file)
  save(dat_acc, dat_kp, dat_ses, file = out_file)
}