# Cluster environment
setwd("~/Documents/Autocorrelation")
# Base directory for the data set we will create
dat_dir <- "/project_cephfs/3022017.02/projects/lorkno/data"

source("src/load_dependencies.R")

### Option: Permute the matrix ownership to generate a null distribution?

PERMUTE <- TRUE

### Option: Number of partitions used to calculate TDIA
n_parts <- 30
# Note: Takes about 5 hours and 15 minutes for n_parts = 30

### Load data

dirs <- list.dirs(dat_dir)

pattern <- "sub-([0-9]+)/preproc$"
preproc_paths <- str_subset(dirs, pattern)
subjects <- str_match(preproc_paths, pattern)[,2]

# Vector of indices for all eligible subjects
sub_idx <- 1:length(subjects)

load(file.path(dat_dir, "dats_kp.rda"))
load(file.path(dat_dir, "dats_ses.rda"))

# A somewhat non-streamlined way to select only those subjects with at least
# 30 days of data. Would be prettier if done with dplyr.
for (i in sub_idx) {
  if (n_distinct(dats_ses[[i]]$date) < 30) {
    # Remove subject index
    sub_idx <- sub_idx[sub_idx != i]
  }
}

subjects <- subjects[sub_idx]
dats_kp <- dats_kp[sub_idx]
dats_ses <- dats_ses[sub_idx]

dists <- vector(mode = "list", length(subjects))
ikds <- vector(mode = "list", length(subjects))

### Calculate overall histogram to get appropriate breaks

# Only use alphanum-to-alphanum key presses for the total histogram

for (i in 1:length(subjects)) {
  dat_kp <- dats_kp[[i]]
  
  dat_kp_alphanum <- dat_kp %>%
    filter(keypress_type == "alphanum", previousKeyType == "alphanum")
  
  dists[[i]] <- dat_kp_alphanum$distanceFromPrevious
  ikds[[i]] <- dat_kp_alphanum$IKD
}

ls <- bin2d(unlist(dists), unlist(ikds), xBins = 100, yBins = 100)
xBreaks <- ls$xBreaks
yBreaks <- ls$yBreaks

### Partition data

n_sub <- length(subjects)

# List of partition data frames for all subjects
subject_partitions <- vector("list", n_sub)

for (i in 1:n_sub) {
  dat_kp <- dats_kp[[i]]
  
  dat_kp_alphanum <- dat_kp %>%
    filter(keypress_type == "alphanum", previousKeyType == "alphanum")
  
  dat_kp_alphanum <- dat_kp_alphanum %>%
    mutate(date = date(keypressTimestampLocal))
  n_days <- n_distinct(dat_kp_alphanum$date)
  
  subject_partitions[[i]] <- dat_kp_alphanum %>%
    group_by(date) %>%
    nest() %>%
    mutate(
      numberOfKeyPresses = nrow(data[[1]]),
      hist = map(data, function(df) {
        binCounts(df$distanceFromPrevious, df$IKD, xBreaks, yBreaks)
      })
    )
}

# Extract histogram matrices from partition data frames
subject_mats <- lapply(subject_partitions, function(df) {
  df$hist
})

### TDIA function

subsampled_tdia <- function(subject_mats, n_parts) {
  n_sub <- length(subject_mats)
  
  # Useful for later reference
  # sampled_idx <- lapply(lapply(subject_mats, length), sample.int, n_parts, TRUE)
  
  # Because we want to sample entire blocks of days (currently max
  # 30 days), we have to calculate eligible starting indices of those blocks,
  # and then sample a single starting index per subject
  block_length <- min(n_parts, 30)
  lens <- sapply(subject_mats, length)
  max_start_idx <- lens - block_length + 1
  
  # Sample a single starting index per subject
  sampled_start_idx <- sapply(max_start_idx, sample.int, size = 1)
  # Expand every start index to a block of indices
  sampled_idx <- lapply(sampled_start_idx, 
                        function(idx) idx:(idx+block_length-1))
  
  # Subsampled matrices
  sub_mats <- map2(subject_mats, sampled_idx, ~ .x[.y])
  
  # Every (unique) pair of matrices represents an edge, and for every edge 
  # we need to compute a correlation
  
  # A coordinate pair [e.g. (i, j)] can also be represented as a single number,
  # i.e. (i - 1) * n_parts + j. To create pairs of coordinate pairs, we can create
  # pairs of these single numbers instead.
  n_mats <- n_sub * n_parts
  coords <- 1:n_mats
  
  if (PERMUTE) {
    # Don't permute within subjects, only permute the subjects themselves.
    # Preserves within-subjects correlation.
    # permuted_idx_within <- replicate(n_sub, sample(1:n_parts), simplify = FALSE)
    permuted_idx_between <- sample.int(n_sub)
    
    # sub_mats <- map2(subject_mats, permuted_idx_within, ~ .x[.y])
    sub_mats <- sub_mats[permuted_idx_between]
  }
  
  # Produces a 10000*n_parts*n_sub array
  mat <- sapply(sub_mats, function(mats) sapply(mats, as.vector), 
                simplify = "array")
  dim(mat) <- c(100*100, n_parts*n_sub)
  
  # Old permutation strategy:
  # if (PERMUTE) {
  #   # Permuting the columns of mat will permute the ownership of the individual
  #   # histogram matrices.
  #   permuted_idx <- sample(1:(n_parts*n_sub))
  #   mat <- mat[,permuted_idx]
  # }
  
  cors <- cor(mat)
  
  # Set diagonal to -10 instead of 1, so we won't have to take into account the
  # correlation with the source matrix itself when assessing the highest cor-
  # relations
  diag(cors) <- -10
  
  tdia <- matrix(nrow = n_sub, ncol = n_parts)
  
  for (x in coords) {
    # Select the n_parts highest correlations (the first one will be the
    # correlation of source with itself)
    # lowest_top <- sort(cors[x,], decreasing = TRUE)[n_parts]
    
    # Select the n_parts-1 highest correlations (the first one will no longer 
    # be the correlation of source with itself)
    lowest_top <- sort(cors[x,], decreasing = TRUE)[n_parts - 1]
    
    match_idx <- which(cors[x,] >= lowest_top, arr.ind = TRUE)
    
    # Get subject index from x (calculate row number of the n_sub*n_parts matrix) 
    # and match_idx (also row number of that matrix)
    sub_idx <- (x-1) %/% n_parts + 1
    match_idx <- (match_idx-1) %/% n_parts + 1
    
    part_idx <- (x-1) %% n_parts + 1
    
    if (PERMUTE) {
      # Get actual subject number; otherwise we'd be comparing the permuted
      # subject index to the permuted match index, which is like doing no
      # permutation at all.
      sub_idx <- permuted_idx_between[sub_idx]
      
      # Do the same for the partition number. It is unlikely to matter much,
      # but perhaps it is useful for downstream analysis.
      # part_idx <- permuted_idx_within[[sub_idx]][part_idx]
    }
    
    # Subtract one from the match count to disregard the match of the source
    # matrix with itself
    # tdia[sub_idx, part_idx] <- (sum(match_idx == sub_idx) - 1) / (n_parts - 1)
    
    # We no longer need to subtract 1 because we set diag(cors) to -10
    tdia[sub_idx, part_idx] <- sum(match_idx == sub_idx) / (n_parts - 1)
  }
  
  list("tdia" = tdia, "sampled_idx" = sampled_idx)
}

### Bootstrapping
n_threads <- 10
n_iter <- 10000

cl <- makeCluster(n_threads)
clusterSetRNGStream(cl, NULL) # Make sure every node uses a different RNG stream

# Propagate these to all the child nodes. Seems to save memory w.r.t. forking
# processes
clusterExport(cl, c("subsampled_tdia", "subject_mats", "n_parts", 
                    "PERMUTE", "map2"))

f <- function(iter) {
  subsampled_tdia(subject_mats, n_parts)
}

start <- Sys.time()

tdias <- parSapply(cl, 1:n_iter, f)

print(Sys.time() - start)

stopCluster(cl)

out_file <- file.path(dat_dir, str_glue("tdias_{n_parts}.rda"))

if (PERMUTE) {
  out_file <- str_replace(out_file, "\\.rda", "_permuted_blocked.rda")
}

save(tdias, file = out_file)