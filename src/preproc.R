library(gsignal)
library(dplyr)

# Preprocess the accelerometer data
preproc_acc <- function(path, verbose = FALSE) {
  if (verbose) print("Reading accelerometer data...")
  
  dat_acc <- read.csv(path)
  
  dat_acc <- dat_acc %>%
    mutate(
      date = as.Date(date, format = "%Y-%m-%d"),
      sessionTimestampLocal = as.POSIXct(sessionTimestampLocal, 
                                         format = "%Y-%m-%d %H:%M:%OS")
    )
  
  if (verbose) print("Filtering...")
  
  # Discrete Butterworth filter
  but <- butter(2, w = 0.8, plane = "z", type = "low", output = "Sos")
  
  dat_acc <- dat_acc %>%
    group_by(sessionNumber) %>%
    mutate(
      xFiltered = filtfilt(but, x),
      yFiltered = filtfilt(but, y),
      zFiltered = filtfilt(but, z)
    )
  
  if (verbose) print("Marking as active/upright...")
  
  dat_acc <- dat_acc %>%
    mutate(
      magnitude = sqrt(xFiltered^2 + yFiltered^2 + zFiltered ^2),
      activeSample = magnitude < 0.95 | magnitude > 1.05
    ) %>%
    group_by(sessionNumber) %>%
    mutate(
      activeSes = sum(activeSample) / n() > 0.08,
      xMedian = median(xFiltered),
      zMedian = median(zFiltered),
      upright = zMedian < 0.1 & xMedian >= -0.2 & xMedian <= 0.2,
      bed = !(activeSes | upright)
    )
  
  if (verbose) print("Done with accelerometer data!")
  
  return(dat_acc)
}

# Preprocess the key press and accelerometer data, combine them
preproc <- function(kp_path, acc_path, verbose = FALSE) {
  # Read and preprocess accelerometer data
  dat_acc <- preproc_acc(acc_path, verbose)
  
  # Aggregate accelerometer data over sessions
  dat_acc_ses <- dat_acc %>%
    group_by(sessionNumber) %>%
    summarize(
      active = activeSes[1],
      upright = upright[1],
      bed = bed[1]
    )
  
  if (verbose) print("Reading key press data...")
  dat_kp <- read.csv(kp_path)
  
  dat_kp <- dat_kp %>%
    mutate(
      date = as.Date(date, format = "%Y-%m-%d"),
      keypressTimestampLocal = as.POSIXct(keypressTimestampLocal, 
                                          format = "%Y-%m-%d %H:%M:%OS"),
      sessionTimestampLocal = as.POSIXct(keypressTimestampLocal, 
                                         format = "%Y-%m-%d %H:%M:%OS")
    )
  
  if (verbose) print("Aggregating key press data...")
  # Aggregate over sessions, join with accelerometer data
  dat_ses <- dat_kp %>%
    group_by(sessionNumber) %>%
    mutate(
      autocorrectRate = sum(keypress_type == "autocorrection") / n(),
      backspaceRate = sum(keypress_type == "backspace") / n(),
      totalKeyPresses = n()
    ) %>%
    left_join(dat_acc_ses, by = "sessionNumber") %>%
    filter(keypress_type == "alphanum", previousKeyType == "alphanum") %>%
    summarize(
      medianIKD = median(IKD, na.rm = TRUE),
      percent95IKD = quantile(IKD, .95, na.rm = TRUE),
      madIKD = mad(IKD, na.rm = TRUE),
      autocorrectRate = autocorrectRate[1],
      backspaceRate = backspaceRate[1],
      sessionTimestampLocal = sessionTimestampLocal[1],
      totalKeyPresses = totalKeyPresses[1],
      active = active[1],
      upright = upright[1],
      bed = bed[1],
      hour = format(sessionTimestampLocal, "%H"),
      date = as.Date(sessionTimestampLocal)
    )
  
  if (verbose) print("Done with key press data!")
  
  # Return non-aggregated and aggregated data
  return(list("dat_kp" = dat_kp, "dat_ses" = dat_ses))
}