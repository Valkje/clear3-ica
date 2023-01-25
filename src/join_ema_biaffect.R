local <- TRUE

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

source("src/load_dependencies.R")

### Clean EMA data

dat_rep_full <- read_sav(file.path(dat_dir, "clear3daily_20221205.sav"))
dat_rep_full <- zap_labels(dat_rep_full)

# Add some summary measures first.

dat_rep_summ <- dat_rep_full %>%
  mutate(
    suicidality_mean = rowMeans(select(., 
                                       ASIQ9_wishdead, 
                                       ASIQ2_thoughtkill, 
                                       ASIQ16_thoughtways, 
                                       ASIQ19_lifenotworth, 
                                       ASIQ1_betternotalive,
                                       ASIQ25_notbetterkill,
                                       ASIQ17_thoughtkillnotdo),
                                na.rm = TRUE),
    PMDDemosx_mean = rowMeans(select(.,
                                     DRSP1_depblue,
                                     DRSP2_hopeless,
                                     DRSP3_worthguilt,
                                     DRSP4_anxious,
                                     DRSP5_moodswings,
                                     DRSP6_rejsens,
                                     DRSP7a_angry,
                                     DRSP7b_irritable,
                                     DRSP8_intconflict),
                              na.rm = TRUE),
    environment_int = rowMeans(select(., 
                                      DRSP22_workint, 
                                      DRSP23_hobbsocint, 
                                      DRSP24_relint),
                               na.rm = TRUE),
    rumination = rowMeans(select(.,
                                 RNT_FEEL, 
                                 RNT_repetitiveness, 
                                 RNT_uncontrollable, 
                                 angrum),
                          na.rm = TRUE),
    agitation = rowMeans(select(., starts_with("BAM")), na.rm = TRUE),
    impulsivity = rowMeans(select(., 
                                  actedwothink, 
                                  imp_trouble, 
                                  imp_regret), 
                           na.rm = TRUE),
    irritability_mean = rowMeans(select(., starts_with("BITe")), na.rm = TRUE)) %>%
  rowwise() %>%
  mutate(
    n_stressors = case_when(
      all(is.na(c_across(stress_1:stress_15))) ~ NA_real_,
      TRUE ~ sum(c_across(stress_1:stress_15), na.rm = TRUE)
    )
  ) %>%
  ungroup()

# Filter out columns that are not suited for a regression analysis. Reasons for
# exclusion are given in the excel file.

inclusion <- read_excel("regression_inclusion_20221205.xlsx")$Included
dat_rep_reg <- dat_rep_summ[inclusion == 1]

# Change the encoding of some columns (e.g. to factors, or boolean). Likert
# scales will be left as-is. The mhpYN_[n] columns seem to require some special
# handling: When one of the variables has a value of 1, the others tend to have
# `NA`s. (Note that there are some cases where that assumption doesn't hold, as
# shown by a contingency table. If all mhpYN values are NA, I suspect there has
# been a data collection error.) I will convert `NA`s to 0 only if any of the
# other mhpYN variables do not have an NA value. As for firstdayofperiod, we can
# see that it is `NA` whenever menstrualbleeding is 0. I can understand why this
# is the case (because firstdayofperiod is not a useful measure if someone is
# not even on their period), but it messes with methods using complete case
# analysis, so I will change those `NA` cases to 0.

# Replace NAs in partially complete cases
repl_part_comp <- function(col, value, ...) {
  other_cols <- list(...)
  n <- length(other_cols[[1]])
  
  case_when(
    is.na(col) & sapply(1:n, function(i) {
      !all(sapply(other_cols, function(col) is.na(col[i])))
    }) ~ value,
    TRUE ~ col
  )
}

dat_rep_reg <- dat_rep_reg %>%
  mutate(
    id = factor(id),
    firstdayofperiod = case_when(
      menstrualbleeding == 0 ~ 0,
      TRUE ~ firstdayofperiod
    ),
    workday = workday == 1,
    mhpYN_0 = repl_part_comp(mhpYN_0, value = 0, mhpYN_1, mhpYN_2),
    mhpYN_1 = repl_part_comp(mhpYN_1, value = 0, mhpYN_0, mhpYN_2),
    mhpYN_2 = repl_part_comp(mhpYN_2, value = 0, mhpYN_0, mhpYN_1),
    usedPRN = usedPRN == 1,
    NSSIyn = factor(NSSIyn),
    MJuse = MJuse == 1
  )

# As for the 05-12-2022 snapshot of the data set, we see many spurious days for
# a couple of subjects (with `daterated` already starting in 1990). Those days
# contain no actual data, so that makes it easier to exclude them from the data
# set.

all_missing <- data.frame(is.na(dat_rep_reg)) %>%
  rowwise() %>%
  summarize(allMissing = all(c_across(menstrualbleeding:panicattack)))

dat_rep_reg <- dat_rep_reg[!all_missing$allMissing,]

### Join with TDIAs

# Load TDIA data.

load(file.path(dat_dir, "tdias_30_blocked.rda"))

# Load subject key press data.

ls <- load_subject_key_data(dat_dir)
dats_kp <- ls$dats_kp
dats_ses <- ls$dats_ses
subjects <- ls$subjects

# Join the dates and TDIAs on subject

sub_tdia <- link_tdia_date(tdias, subjects, dats_kp)

# Join TDIA with self-report measures based on date and subject

dat_reg <- sub_tdia %>%
  full_join(dat_rep_reg, by = c("subject" = "id", "date" = "daterated"))

### Join with other BiAffect data

# Get all subject session data, regardless of how many data they have
ls <- load_subject_key_data(dat_dir, load_kp = FALSE, min_days = 0)

# Aggregate BiAffect measures per day
names(ls$dats_ses) <- ls$subjects
dat_ses <- bind_rows(ls$dats_ses, .id = "subject")

dat_day <- dat_ses %>%
  group_by(subject, date) %>%
  summarize(
    across(medianIKD:backspaceRate, mean),
    totalKeyPresses = sum(totalKeyPresses),
    across(active:bed, mean)
  )

# Join BiAffect features with self-report measures based on date and subject

dat_reg <- dat_day %>%
  full_join(dat_reg, by = c("subject", "date"))

### Save (unscaled) cleaned data to file.

save(dat_reg, file = file.path(dat_dir, "dat_reg_tdia_30_blocked.rda"))
