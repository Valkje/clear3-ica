library(gsignal)
library(dplyr)
library(stringr)
library(ggplot2) # For calculating 2D histograms
library(lubridate)

# Preprocess the accelerometer data
preproc_acc <- function(raw_acc, verbose = FALSE) {
  if (verbose) print("Converting dates and times to objects...")
  
  dat_acc <- raw_acc %>%
    mutate(
      sessionTimestampLocal = as_datetime(sessionTimestamp + createdOnTimeZone * 36),
      sessionNumber = cumsum(sampleNumber == 1)
    ) %>%
    rename(x = xCoord, y = yCoord, z = zCoord) %>%
    select(!c(sessionTimestamp, sessionTimeZone, sampleNumber))
  
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
preproc_kp <- function(raw_kp, dat_acc, verbose = FALSE) {
  # Small wrapper around boolean test and print
  vprint <- function(text) if (verbose) print(text)
  
  vprint("Aggregating accelerometer data...")
  
  # Aggregate accelerometer data over sessions
  dat_acc_ses <- dat_acc %>%
    group_by(sessionNumber) %>%
    summarize(
      active = activeSes[1],
      upright = upright[1],
      bed = bed[1]
    )
  
  vprint("Converting key press dates and times to objects...")
  
  dat_kp <- raw_kp %>%
    mutate(
      sessionNumber = cumsum(sampleNumber == 1),
      sessionTimestampLocal = as_datetime(sessionTimestamp + createdOnTimeZone * 36),
      keypressTimestampLocal = as_datetime(timestamp + createdOnTimeZone * 36),
      phoneType = paste(
        "iPhone",
        str_replace(
          str_match(
            phoneInfo, 
            "iPhone\\s?(\\w+(?:\\s\\w+|\\+)?(?:\\s\\w+)?)-?,?")[,2],
          "\\+", " Plus"))
    ) %>%
    group_by(sessionNumber) %>%
    mutate(
      IKD = c(NA, diff(timestamp)),
      previousKeyType = lag(keypress_type),
    ) %>%
    ungroup() %>%
    select(!c(sessionTimestamp, sessionTimeZone, sampleNumber))
  
  vprint("Converting screen point distance to centimeters...")
  
  screen_specs <- read.delim("data/iPhone_screen_specs.tsv", strip.white = TRUE)
  # This assumes that the subject has only used one phone during the study.
  # Having to check the phone type for every row slows down the code considerably.
  spec <- screen_specs %>% 
    filter(phoneType == dat_kp[1,]$phoneType)
  
  # points_to_cm <- function(dist) {
  #   if (is.na(dist) || is.na(pType)) {
  #     return(NA)
  #   }
  #   
  #   # pType <- str_match(phoneType, "iPhone(\\s?\\w+(?:\\s\\w+)?(?:\\s\\w+)?)-?")[,2]
  #   # pType <- paste("iPhone", pType)
  #   
  #   # spec <- screen_specs %>% filter(phoneType == pType)
  #   
  #   # Multiply with scale factor to get physical pixels, divide by ppi to
  #   # get inches, convert to centimeters by multiplying with 2.54
  #   return(dist * spec$scaleFactor / spec$PPI * 2.54)
  # }
  
  dat_kp <- dat_kp %>%
    mutate(
      distanceFromPrevious = distanceFromPrevious * spec$scaleFactor / spec$PPI * 2.54,
      distanceFromCenter = distanceFromCenter * spec$scaleFactor / spec$PPI * 2.54,
    )
  
  vprint("Aggregating key press data...")
  
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
  
  vprint("Done with key press data!")
  
  # Return non-aggregated and aggregated data
  return(list("dat_kp" = dat_kp, "dat_ses" = dat_ses))
}

hist_distances <- function(dat_kp, verbose = FALSE) {
  # Small wrapper around boolean test and print
  vprint <- function(text) if (verbose) print(text)
  
  vprint("Calculating 'distanceFromPrevious'-'IKD' histograms and distances between them...")
  dat_kp_alphanum <- dat_kp %>%
    filter(keypress_type == "alphanum", previousKeyType == "alphanum")
  
  # Use ggplot to create 2D histograms
  h <- ggplot(dat_kp_alphanum, aes(x = distanceFromPrevious, y = IKD)) +
    geom_bin_2d(bins = 100)
  hist2d <- ggplot_build(h)$data[[1]]
  # Get bin edges
  edges <- list(x = sort(unique(hist2d$xmin)), y = sort(unique(hist2d$ymin)))
  
  vprint("Calculating entropy distances...")
  # Partition the data in time frames of a month, calculate the normalised 2D 
  # histograms, and then calculate the KL divergence from the first month.
  # We denote the overall histogram as our model distribution $Q$, and all 
  # other, time-windowed histograms as our observation distributions $P$.
  
  # Recalculate the overall histogram, because specifying the breaks instead of
  # the bins yields slightly different histograms
  histQ <- ggplot_build(
    ggplot(dat_kp_alphanum, aes(x = distanceFromPrevious, y = IKD)) +
      geom_bin_2d(breaks = edges))$data[[1]]
  
  hists <- dat_kp_alphanum %>%
    group_by(
      year = year(sessionTimestampLocal),
      week = week(sessionTimestampLocal)
    ) %>%
    nest() %>%
    mutate(
      hist = map(data, function (df) {
        h <- ggplot_build(
          ggplot(df, aes(x = distanceFromPrevious, y = IKD)) +
            geom_bin_2d(breaks = edges)
        )
        
        h$data[[1]]
      }),
      entropy = map_dbl(hist, function(hist) {
        sum(hist$density * log2(1 / hist$density))
      }),
      KL_divergence = map_dbl(hist, function(hist) {
        joint <- hist %>%
          full_join(histQ, by = c("xbin", "ybin"), suffix = c(".P", ".Q")) %>%
          filter(!is.na(density.P))
        
        sum(joint$density.P * log2(joint$density.P / joint$density.Q))
      })
    )
  
  vprint("Calculating Bhattacharyya and Hellinger distances...")
  
  bhat <- vector("double", nrow(hists))
  hell <- vector("double", nrow(hists))
  for (i in 2:nrow(hists)) {
    # Compare between week i-1 and i
    hist_prev <- hists[i-1,]$hist[[1]]
    hist_cur <- hists[i,]$hist[[1]]
    
    joint <- hist_cur %>%
      full_join(hist_prev, by = c("xbin", "ybin"), suffix = c(".cur", ".prev")) %>%
      filter(!(is.na(density.cur) | is.na(density.prev)))
    
    # Bhattacharyya coefficient
    bc <- sum(sqrt(joint$density.cur * joint$density.prev))
    
    bhat[i] <- -log(bc)
    hell[i] <- sqrt(1 - bc)
  }
  
  hists$bhattacharyya <- bhat
  hists$hellinger <- hell
  
  vprint("Merging histogram distance and key press data...")
  dat_dist <- dat_ses %>% 
    filter(totalKeyPresses > 10) %>%
    group_by(
      year = year(sessionTimestampLocal), 
      # month = month(sessionTimestampLocal), 
      week = week(sessionTimestampLocal)
    ) %>%
    summarise(
      medianIKD = mean(medianIKD),
      percent95IKD = mean(percent95IKD),
      madIKD = mean(madIKD),
      autocorrectRate = mean(autocorrectRate),
      backspaceRate = mean(backspaceRate),
      active = mean(active),
      upright = mean(upright)
    ) %>%
    ungroup() %>%
    left_join(hists %>%
                filter(nrow(data[[1]]) > 2000) %>%
                select(year, 
                       week, 
                       entropy, 
                       KL_divergence, 
                       bhattacharyya, 
                       hellinger),
              by = c("year", "week"))
  
  return(dat_dist)
}