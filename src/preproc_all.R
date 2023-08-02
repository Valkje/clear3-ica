rearrange_raw_files <- function(skip_src_transfer) {
  if (skip_src_transfer)
    return()

  # The completely unmodified PSV files, as received from Faraz
  src_dir <- "../CLEAR/"

  ### Move all source files into a BIDS-like structure
  src_kp_dir <- file.path(src_dir, "Clear lab participants keystroke dynamics data")
  src_acc_dir <- file.path(src_dir, "Clear lab participants keyboard accelerometer kinematics data")

  # Transfer files that are non-empty

  pat <- "\\+(3[0-9]+)"

  kp_files <- list.files(src_kp_dir, full.names = TRUE)
  kp_files <- str_subset(kp_files, pat)
  subjects <- str_match(kp_files, pat)[,2]

  acc_files <- list.files(src_acc_dir, full.names = TRUE)
  acc_files <- str_subset(acc_files, pat)
  subjects_acc <- str_match(acc_files, pat)[,2]

  if (any(subjects != subjects_acc)) {
    stop("Key press files do not match up with accelerometer files")
  }

  # The first line of the PSV key stroke files contains the column headers, but
  # while there are 15 columns, quite often there are only 13 headers, for reasons
  # unknown to me. Some of the code below tries to fix that.
  headers = c("distanceFromCenter", "distanceFromPrevious")
  intendend_n_pipes <- 14

  report_file <- file.path(dat_dir, "preproc_report.csv")
  write_lines("subject,keystroke,accelerometer", report_file)

  for (i in 1:length(subjects)) {
    sub <- subjects[i]
    kp_path <- kp_files[i]
    acc_path <- acc_files[i]

    sub_dir <- str_glue("sub-{sub}")
    raw_path <- file.path(dat_dir, sub_dir, "raw")

    print(str_glue("Processing source key press files for subject {sub}"))

    # Many key press files are missing the last two column headers,
    # so we will add them here if that's the case

    lin <- read_lines(kp_path)

    kp_present <- 0
    # If the file is empty (except for the headers), skip this step
    if (length(lin) != 1) {
      kp_present <- 1

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

    acc_present <- 0
    if (length(lin) != 1) {
      acc_present <- 1

      dir.create(raw_path, showWarnings = FALSE, recursive = TRUE)

      sub_acc_file <- file.path(raw_path, str_glue("sub-{sub}_accelerometer.psv"))
      file.copy(acc_path, sub_acc_file, overwrite = TRUE)
    }

    # So we know what data are present
    write_lines(paste(sub, kp_present, acc_present, sep = ","),
                report_file,
                append = TRUE)
  }
}

preproc_biaffect <- function() {
  ### Listing available raw data files
  dirs <- list.dirs(dat_dir)

  pattern <- "sub-([0-9]+)/raw$"
  raw_paths <- str_subset(dirs, pattern)
  subjects <- str_match(raw_paths, pattern)[,2]

  ### Preprocessing for every subject that has both key press and accelerometer
  ### data

  n_sub <- length(subjects)
  dats_acc <- vector("list", n_sub)
  dats_kp <- vector("list", n_sub)
  dats_ses <- vector("list", n_sub)

  for (i in 1:n_sub) {
    tryCatch(
      {
        sub <- subjects[i]

        print(str_glue("Preprocessing subject {sub}"))

        raw_path <- str_subset(raw_paths, str_glue("sub-{sub}"))
        kp_path <- file.path(raw_path, str_glue("sub-{sub}_keystrokes.psv"))
        acc_path <- file.path(raw_path, str_glue("sub-{sub}_accelerometer.psv"))

        print("Reading data...")
        raw_kp <- read.table(kp_path, header = TRUE, sep = "|", strip.white = TRUE, fill = TRUE)
        raw_acc <- read.table(acc_path, header = TRUE, sep = "|", strip.white = TRUE, fill = TRUE)

        print("Preprocessing accelerometer and key press data...")
        dat_acc <- preproc_acc(raw_acc, sub)

        dats <- preproc_kp(raw_kp, dat_acc, sub)
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

        # Makes it easier to load all subject data in one go
        dats_acc[[i]] <- dat_acc
        dats_kp[[i]] <- dat_kp
        dats_ses[[i]] <- dat_ses
      },
      error = function(e) {
        message(str_glue("Error occurred when processing subject {sub}:"))
        message(e)
        message(str_glue("Skipping subject {sub}."))
      }
    )
  }

  dats_acc <- compact(dats_acc)
  dats_kp <- compact(dats_kp)
  dats_ses <- compact(dats_ses)

  # Save using high compression and after removing potential NULL entries
  save(dats_acc, file = file.path(dat_dir, "dats_acc.rda"), compress = "xz")
  save(dats_kp, file = file.path(dat_dir, "dats_kp.rda"), compress = "xz")
  save(dats_ses, file = file.path(dat_dir, "dats_ses.rda"), compress = "xz")
}

# Only runs when called from the command line
if (sys.nframe() == 0L) {
  local <- TRUE

  wd <- ""

  if (!local) {
    # Cluster environment
    wd <- "~/Documents/Autocorrelation"
    # Base directory for the data set we will create
    dat_dir <- "/project_cephfs/3022017.02/projects/lorkno/data"
  } else {
    wd <- "/Volumes/home/preclineu/lorkno/Documents/Autocorrelation"
    # Base directory for the data set we will create
    dat_dir <- "~/HPC_project/data"
  }

  # For some reason this sets the pwd only for the current scope, which means it
  # does not affect anything outside an if block if you set it there.
  # So that's why it's here instead of above.
  setwd(wd)

  source("src/load_dependencies.R")

  dir.create(dat_dir, showWarnings = FALSE, recursive = TRUE)

  skip_src_transfer <- TRUE

  rearrange_raw_files(skip_src_transfer)

  preproc_biaffect()
}