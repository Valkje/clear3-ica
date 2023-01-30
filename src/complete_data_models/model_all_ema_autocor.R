# Cluster environment
setwd("~/Documents/Autocorrelation")
# Base directory for the data set we will create
dat_dir <- "/project_cephfs/3022017.02/projects/lorkno/data"

source("src/load_dependencies.R")

model_dir <- file.path(dat_dir, "complete_data_models")

load(file.path(dat_dir, "imp_multi_tdia_30_blocked.rda"))

# For some reason these models become absolutely massive: With about 50 GB of
# RAM I can save them just fine, but when reading them back in even 128 GB of
# RAM is not enough. I will therefore iterate over all the columns that have to
# be modelled and save them one by one.

out_dir <- file.path(model_dir, "models_all_ema_autocor")
dir.create(out_dir, showWarnings = FALSE)

imp_comp <- mice::complete(imp_multi, "long")

columns <- names(imp_comp)
start_idx <- which(columns == "menstrualbleeding")
end_idx <- which(columns == "n_stressors")

for (i in start_idx:end_idx) {
  ema_col <- columns[i]
  
  out_path <- file.path(out_dir, 
                        str_glue("models_all_ema_autocor_{ema_col}.rds"))
  
  if (file.exists(out_path) || is.factor(imp_comp[,ema_col])) {
    next
  }
  
  # For menstrualbleeding, this object is about 1 GB
  dat <- imp_comp %>%
    group_by(.imp) %>%
    summarize(
      model = list(lme(!!sym(ema_col) ~ mean_accuracy +
                         medianIKD +
                         percent95IKD +
                         madIKD +
                         autocorrectRate +
                         backspaceRate +
                         totalKeyPresses +
                         active +
                         upright +
                         bed,
                       random = ~ 1 | subject,
                       correlation = corAR1(),
                       data = cur_data()))
    )
  
  saveRDS(dat, out_path)
}

# models_all_ema_autocor <- mice::complete(imp_multi, "long") %>%
#   select(!NSSIyn) %>%
#   group_by(.imp) %>%
#   summarize(across(menstrualbleeding:n_stressors,
#                    ~ list(lme(.x ~ mean_accuracy +
#                                 medianIKD +
#                                 percent95IKD +
#                                 madIKD +
#                                 autocorrectRate +
#                                 backspaceRate +
#                                 totalKeyPresses +
#                                 active +
#                                 upright +
#                                 bed,
#                               random = ~ 1 | subject,
#                               correlation = corAR1(),
#                               data = cur_data()))))
# 
# path <- file.path(dat_dir, "complete_data_models", "models_all_ema_autocor.rds")
# saveRDS(models_all_ema_autocor, file = path)