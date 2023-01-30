local <- FALSE

if (!local) {
  # Cluster environment
  setwd("~/Documents/Autocorrelation")
  # Base directory for the data set we will create
  dat_dir <- "/project_cephfs/3022017.02/projects/lorkno/data"
} else {
  setwd("~/HPC/Documents/Autocorrelation")
  # Base directory for the data set we will create
  dat_dir <- "~/HPC_project/data"
}

model_dir <- file.path(dat_dir, "complete_data_models")

source("src/load_dependencies.R")

col_files <- list.files(file.path(model_dir, "models_all_ema_autocor"),
                        full.names = TRUE)

ema_cols <- str_match(col_files, "models_all_ema_autocor_(.*)\\.rds")[,2]

all_pools <- vector("list", length(ema_cols))
names(all_pools) <- ema_cols

for (i in 1:length(col_files)) {
  col_models <- readRDS(col_files[[i]])
  all_pools[[i]] <- pool(col_models$model)
}

summs <- vector("list", length(all_pools))
for (i in 1:length(all_pools)) {
  summs[[i]] <- summary(all_pools[[i]])

  if (any(summs[[i]]$p.value <= 0.05)) {
    print(ema_cols[i])
    print(summs[[i]])
    print("======================")
  }
}

save(all_pools, summs,
     file = file.path(dat_dir, "pooling", "pooled_all_ema_autocor.rda"))