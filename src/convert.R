# Convert BiAffect RDA files to CSV

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

# No restriction on minimum number of days
ls <- load_subject_key_data(dat_dir, min_days = 1)

subjects <- ls$subjects
dats_kp <- ls$dats_kp
dats_ses <- ls$dats_ses

n_sub <- length(subjects)

pb <- txtProgressBar(max = n_sub, style = 3)

for (i in 1:n_sub) {
  setTxtProgressBar(pb, i)

  sub <- subjects[i]

  preproc_path <- file.path(dat_dir, str_glue("sub-{sub}"), "preproc")

  dat_kp_file <- file.path(preproc_path, str_glue("sub-{sub}_dat-kp.csv"))
  if (!file.exists(dat_kp_file))
    write.csv(dats_kp[i], dat_kp_file)

  dat_ses_file <- file.path(preproc_path, str_glue("sub-{sub}_dat-ses.csv"))
  if (!file.exists(dat_ses_file))
    write.csv(dats_ses[i], dat_ses_file)
}

close(pb)