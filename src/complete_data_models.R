# Takes just over 01:35 hours to run

# Cluster environment
setwd("~/Documents/Autocorrelation")
# Base directory for the data set we will create
dat_dir <- "/project_cephfs/3022017.02/projects/lorkno/data"

source("src/load_dependencies.R")

### Loading data

load(file.path(dat_dir, "imp_multi_tdia_30_blocked.rda"))
# load(file.path(dat_dir, "dat_reg_scales_tdia_30_blocked.rda"))

# Before we did the imputation, we had to subtract the means from some of the
# variables to make it work with the mixed imputation models. We now add them
# back (at least temporarily).
imputed_models <- mice::complete(imp_multi, "long") %>%
  select(!NSSIyn) %>%
  # mutate(
  #   across(.fns = function(x) {
  #     if (cur_column() %in% names(centers)) {
  #       return(scales[cur_column()] * x + centers[cur_column()])
  #     }
  #     
  #     x
  #   })
  # ) %>%
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

### Rescaling mean_accuracy, restructuring imputed data sets

# mean_acc <- centers["mean_accuracy"]

# 50 rows, with a sub-data-frame (imputed data set) in column `data`
# all_imps <- mice::complete(imp_multi, "long") %>%
#   mutate(mean_accuracy = mean_accuracy + mean_acc) %>%
#   group_by(.imp) %>%
#   nest()

### Parallel model estimation

# n_threads <- 10
# 
# cl <- makeCluster(n_threads)
# clusterSetRNGStream(cl, NULL) # Make sure every node uses a different RNG stream
# 
# # Set up every worker
# clusterEvalQ(cl, {
#   library(lme4)
#   NULL
# })
# 
# start <- Sys.time()
# 
# imputed_models <- parSapply(cl, all_imps$data, function(data) {
#   glmer(mean_accuracy ~ . - subject - .id + (1 | subject), 
#         data = data,
#         family = "binomial",
#         control = glmerControl("bobyqa",
#                                optCtrl = list(
#                                  maxfun = 11 * ncol(data)^2
#                                )))
# })
# 
# print(Sys.time() - start)
# 
# stopCluster(cl)

save(imputed_models, file = file.path(dat_dir, "imputed_models_tdia_30_blocked_centered_scaled.rda"))
