# Cluster environment
setwd("~/Documents/Autocorrelation")
# Base directory for the data set we will create
dat_dir <- "/project_cephfs/3022017.02/projects/lorkno/data"

source("src/load_dependencies.R")

load(file.path(dat_dir, "imp_multi_tdia_30_blocked.rda"))

models_all_ema <- mice::complete(imp_multi, "long") %>%
  select(!NSSIyn) %>%
  group_by(.imp) %>%
  summarize(across(menstrualbleeding:n_stressors,
                   ~ list(lmer(.x ~ mean_accuracy +
                                 medianIKD +
                                 percent95IKD +
                                 madIKD +
                                 autocorrectRate +
                                 backspaceRate +
                                 totalKeyPresses +
                                 active +
                                 upright +
                                 bed +
                                 (1 | subject)))))

path <- file.path(dat_dir, "complete_data_models", "models_all_ema.rds")
saveRDS(models_all_ema, file = path)
