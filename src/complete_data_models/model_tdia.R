# Takes just over 01:35 hours to run

# Cluster environment
setwd("~/Documents/Autocorrelation")
# Base directory for the data set we will create
dat_dir <- "/project_cephfs/3022017.02/projects/lorkno/data"

source("src/load_dependencies.R")

load(file.path(dat_dir, "imp_multi_tdia_30_blocked.rda"))
load(file.path(dat_dir, "dat_reg_scales_tdia_30_blocked.rda"))

all_imps <- mice::complete(imp_multi, "long") %>%
  select(!NSSIyn) %>%
  group_by(.imp) %>%
  nest()

### Rescaling mean_accuracy, restructuring imputed data sets

mean_acc <- centers["mean_accuracy"]

# 50 rows, with a sub-data-frame (imputed data set) in column `data`
all_imps <- mice::complete(imp_multi, "long") %>%
  mutate(mean_accuracy = mean_accuracy + mean_acc) %>%
  group_by(.imp) %>%
  nest()

### Parallel model estimation

n_threads <- 10

cl <- makeCluster(n_threads)
clusterSetRNGStream(cl, NULL) # Make sure every node uses a different RNG stream

# Set up every worker
clusterEvalQ(cl, {
  library(lme4)
  NULL
})

start <- Sys.time()

models_tdia <- parSapply(cl, all_imps$data, function(data) {
  glmer(mean_accuracy ~ . - subject - .id + (1 | subject),
        data = data,
        family = "binomial",
        control = glmerControl("bobyqa",
                               optCtrl = list(
                                 maxfun = 11 * ncol(data)^2
                               )))
})

print(Sys.time() - start)

stopCluster(cl)

path <- file.path(dat_dir, "complete_data_models", "models_tdia.rds")
saveRDS(models_tdia, file = path)
