library(fastICA)
library(grid)

# Because it's required by the Lancet
options(OutDec = "·")

run_icas <- function(dat_path, ns_comp, out_path = NULL, sub_norm = FALSE) {
  set.seed(42)

  ### Data prep

  dat_reg <- readRDS(dat_path)

  dat_reg_sr <- dat_reg %>%
    filter(modality == "self_report", !is.na(value)) %>%
    select(!c(study_start_date, treatment_start_date, modality)) %>%
    group_by(variable) %>%
    mutate(
      logged = case_when(
        min(value) == 0 ~ log(value + 1), # To prevent log(0) = NA values
        TRUE ~ log(value)
      ),
      scaled = scale(value, scale = FALSE)
    ) %>%
    ungroup()

  if (sub_norm) {
    # Normalize variance within subjects
    dat_reg_sr <- dat_reg_sr %>%
      group_by(subject, variable) %>%
      mutate(logged = scale(logged, scale = FALSE)) %>%
      ungroup()
  }

  df <- dat_reg_sr %>%
    select(!c(value, scaled)) %>%
    pivot_wider(names_from = variable, values_from = logged) %>%
    arrange(subject, date)

  subs <- df$subject
  dates <- df$date

  df <- df %>%
    select(!c(subject, date))

  # t x q
  mat <- sapply(df, as.vector)

  means <- colMeans(mat)

  ### Running the ICAs

  icas <- vector("list", length(ns_comp))
  names(icas) <- ns_comp

  for (i in 1:length(ns_comp)) {
    icas[[i]] <- fastICA(mat, ns_comp[i], alg.typ = "parallel", fun = "logcosh")
  }

  if (!is.null(out_path))
    save(icas, subs, dates, file = out_path)

  icas
}

mat_names <- c(
  "ASIQ9_wishdead",
  "wishsleep",
  "ASIQ2_thoughtkill",
  "ASIQ16_thoughtways",
  "ASIQ4_thoughtwhen",
  "ASIQ3_thoughthow",
  "wantedkill",
  "ASIQ19_lifenotworth",
  "ASIQ1_betternotalive",
  "ASIQ25_notbetterkill",
  "ASIQ17_thoughtkillnotdo",
  "DRSP1_depblue",
  "DRSP2_hopeless",
  "DRSP3_worthguilt",
  "DRSP4_anxious",
  "DRSP5_moodswings",
  "DRSP6_rejsens",
  "DRSP8_intconflict",
  "DRSP9_lessint",
  "DRSPx_notenjoy",
  "DRSPx_unmotivated",
  "DRSP16_overwhelm",
  "DRSP17_outofcontrol",
  "BAM1",
  "BAM2_stirredupscream",
  "BAM3",
  "BITe1",
  "BITe2",
  "BITe3",
  "BITe4",
  "BITe5",
  "PANAS_happy",
  "belonging_",
  "mastery"
)

var_dict <- list(
  "ASIQ1_betternotalive" = "BetterNotAlive",
  "ASIQ2_thoughtkill" = "ThoughtKillMyself",
  "ASIQ3_thoughthow" = "ThoughtHowKill",
  "ASIQ4_thoughtwhen" = "ThoughtWhenKill",
  "ASIQ9_wishdead" = "WishedWereDead",
  "ASIQ16_thoughtways" = "ThoughtWaysKill",
  "ASIQ17_thoughtkillnotdo" = "ThoughtKillNotDo",
  "ASIQ19_lifenotworth" = "LifeNotWorth",
  "ASIQ25_notbetterkill" = "IfNotBetterKill",
  "BAM1" = "CrawlOutSkin",
  "BAM2_stirredupscream" = "StirredUpWantedScream",
  "BAM3" = "EmotionalTurmoilGut",
  "BITe1" = "Grumpy",
  "BITe2" = "MightSnap",
  "BITe3" = "PeopleOnNerves",
  "BITe4" = "MoreBothered",
  "BITe5" = "Irritable",
  "DRSP1_depblue" = "FeltDepressed",
  "DRSP2_hopeless" = "FeltHopeless",
  "DRSP3_worthguilt" = "FeltWorthless",
  "DRSP4_anxious" = "FeltAnxious",
  "DRSP5_moodswings" = "MoodSwings",
  "DRSP6_rejsens" = "RejectionSensitivity",
  "DRSP8_intconflict" = "InterpersonalConflict",
  "DRSP9_lessint" = "LackingInterest",
  "DRSP16_overwhelm" = "FeltOverwhelmed",
  "DRSP17_outofcontrol" = "FeltOutOfControl",
  "DRSPx_notenjoy" = "Anhedonia",
  "DRSPx_unmotivated" = "Unmotivated",
  "belonging_" = "FeltConnected",
  "mastery" = "FeltCapable",
  "wantedkill" = "WantedKillMyself",
  "wishsleep" = "WishNotWakeUp",
  "PANAS_happy" = "FeltHappy"
)

fancy_mix_fig <- function(ica, n_row = 1, lims = c(-0.5, 0.5), ann_x = NULL,
                          save_path = NULL, width = 16.18, height = 10) {
  cols <- c("lightcoral", "aquamarine3", "turquoise4", "sienna", "darkorange3",
            "tan1", "indianred3", "steelblue")

  A <- ica$A
  mix_df <- data.frame(A) %>%
    rename_with(function(x) mat_names) %>%
    add_column(ic = 1:nrow(A), .before = 1) %>%
    pivot_longer(!ic, names_to = "variable", values_to = "loading") %>%
    mutate( # Add some questionnaire grouping information.
      questionnaire = case_when(
        str_starts(variable, "ASIQ") ~ "ASIQ",
        variable == "belonging_" ~ "INQ",
        str_starts(variable, "BAM") ~ "BAM",
        str_starts(variable, "BITe") ~ "BITe",
        str_starts(variable, "DRSP") ~ "DRSP",
        variable == "mastery" | variable == "wishsleep" | variable == "wantedkill" ~ "Misc",
        variable == "PANAS_happy"  ~ "PANAS"
      ),
      variable = var_dict[variable],
      variable = fct_rev(factor(variable, var_dict, ordered = TRUE)),
      variable_code = as.numeric(variable)
    )

  n_ics <- n_distinct(mix_df$ic)

  # Custom annotation function
  ann <- function(grob,
                  xmin = -Inf, xmax = Inf,
                  ymin = -Inf, ymax = Inf,
                  data) {
    layer(data = data, stat = StatIdentity, position = PositionIdentity,
          geom = ggplot2:::GeomCustomAnn,
          inherit.aes = TRUE, params = list(grob = grob,
                                            xmin = xmin, xmax = xmax,
                                            ymin = ymin, ymax = ymax))
  }

  g <- ggplot(mix_df, aes(loading,
                          variable_code,
                          fill = questionnaire,
                          alpha = abs(loading))) +
    # annotate(geom = "rect", xmin = -0.5, xmax = 0.5, ymin = 0.5, ymax = 9.5, fill = alpha("steelblue", 0.5)) +
    # annotate(geom = "tile", x = 0, y = 11, width = 1, height = 3, fill = alpha("sienna", 0.5)) +
    geom_col(aes(group = questionnaire), orientation = "y") +
    facet_wrap(~ ic, nrow = n_row, labeller = as_labeller(function(x) {
      paste("IC", 1:n_ics)
    })) +
    scale_y_continuous(breaks = 1:max(mix_df$variable_code),
                       labels = levels(mix_df$variable)) +
    # scale_fill_gradient2(low = muted("blue"), mid = "white", high = muted("red"),
    #                      name = "Loading") +
    scale_fill_manual(values = cols, name = "Questionnaire") +
    scale_alpha_continuous(guide = "none") +
    xlim(lims[1], lims[2]) +
    xlab("Loading") +
    ylab("Variable") +
    coord_cartesian(clip = "off") +
    theme(text = element_text(size = 22),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
          # axis.text.y = element_blank(),
          axis.text.y = element_text(size = 10, margin = margin(0, 20, 0, 0)),
          axis.ticks.y = element_blank(),
          plot.margin = unit(c(1, 1, 1, 5), "lines"),
          panel.background = element_rect(fill = "white"),
          panel.grid.major = element_line(color = "lightgrey"),
          strip.background = element_rect("white"))

  # Add fancy annotations
  ics <- 1:n_ics
  ics_per_row <- n_ics / n_row # Assume no remainder
  left_ics <- ics[(ics - 1) %% ics_per_row == 0]
  pan_left_dat <- mix_df %>% filter(ic %in% left_ics) # Only draw the annotation for the left panels

  # Where the colored vertical bars are drawn
  if (is.null(ann_x)) {
    ann_x <- {
      if (n_ics == 5)
        -0.6
      else if (n_ics == 10)
        -0.62
      else
        -0.8
    }
  }

  lwd <- 10

  q_groups <- sort(unique(mix_df$questionnaire))

  for (i in 1:length(q_groups)) {
    q_dat <- mix_df %>%
      filter(questionnaire == q_groups[i]) %>%
      summarize(
        min_var_code = min(variable_code),
        max_var_code = max(variable_code)
      )

    g <- g +
      ann(linesGrob(gp = gpar(col = cols[i], lwd = lwd)),
          xmin = ann_x, xmax = ann_x,
          ymin = q_dat$min_var_code[[1]], ymax = q_dat$max_var_code[[1]],
          data = pan_left_dat)
  }

  if (!is.null(save_path)) {
    ggsave(save_path, g, width = width, height = height)
  }

  g
}



# Only executes if script is run from command line
if (sys.nframe() == 0L) {
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

  model_dir <- file.path(dat_dir, "complete_data_models")

  # For manuscript images
  man_img_dir <- "~/Documents/Writing/Papers/Paper 1/images"

  # For some reason this sets the pwd only for the current scope, which means it
  # does not affect anything outside an if block if you set it there.
  # So that's why it's here instead of above.
  setwd(wd)

  source("src/load_dependencies.R")

  # Semi-contiguous ICA
  icas <- run_icas(dat_path = file.path(dat_dir, "dat_reg_semi_contiguous.rds"),
                   ns_comp = c(5, 10, 20),
                   out_path = file.path(dat_dir, "dat_reg_contig_icas.rda"))

  fancy_mix_fig(icas$`5`,
                save_path = file.path(man_img_dir, "mixing_matrix_5.pdf"))
  fancy_mix_fig(icas$`10`, n_row = 2,
                save_path = file.path(man_img_dir, "mixing_matrix_10.pdf"),
                width = 15, height = 15)
  fancy_mix_fig(icas$`20`, n_row = 2, lims = c(-0.6, 0.6),
                save_path = file.path(man_img_dir, "mixing_matrix_20.pdf"),
                width = 15, height = 15)

  # Fragmented ICA
  ica_frag <- run_icas(file.path(dat_dir, "dat_reg_fragmented.rds"),
                       ns_comp = c(5, 10, 20),
                       out_path = file.path(dat_dir, "dat_reg_frag_icas.rda"))

  fancy_mix_fig(ica_frag[[1]],
                lims = c(-0.6, 0.6),
                ann_x = -0.75,
                save_path = file.path(man_img_dir, "mixing_matrix_5_frag.pdf"))
  fancy_mix_fig(ica_frag[[2]],
                n_row = 2,
                lims = c(-0.6, 0.6),
                ann_x = -0.78,
                save_path = file.path(man_img_dir, "mixing_matrix_10_frag.pdf"),
                width = 15, height = 15)
  fancy_mix_fig(ica_frag[[3]],
                n_row = 2,
                lims = c(-0.6, 0.6),
                ann_x = -0.9,
                save_path = file.path(man_img_dir, "mixing_matrix_20_frag.pdf"),
                width = 15, height = 15)

  # Within-subject mean-centring
  ica_norm <- run_icas(dat_path = file.path(dat_dir, "dat_reg_fragmented.rds"),
                       ns_comp = c(5), sub_norm = TRUE)[[1]]

  fancy_mix_fig(ica_norm,
                lims = c(-0.6, 0.6),
                save_path = file.path(man_img_dir, "mixing_matrix_5_norm.pdf"))
}