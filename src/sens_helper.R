# Prerequisite: icas

library(grid)

fancy_mix_fig_sens <- function(mix_df, cor_idx, n_row = 1, lims = c(-0.5, 0.5),
                          save_path = NULL, width = 16.18, height = 10) {
  cols <- c("lightcoral", "aquamarine3", "turquoise4", "sienna", "darkorange3",
            "tan1", "indianred3", "steelblue")

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
      labels <- paste("IC", 1:n_ics)
      labels[cor_idx] <- paste(labels[cor_idx], "*")
      labels
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
  # ics <- 1:n_ics
  # ics_per_row <- n_ics / n_row # Assume no remainder
  # left_ics <- ics[(ics - 1) %% ics_per_row == 0]
  # pan_left_dat <- mix_df %>% filter(ic %in% left_ics) # Only draw the annotation for the left panels
  #
  # ann_x <- -0.6 - 0.004 * n_ics^2
  # ann_x <- {
  #   if (n_ics == 5)
  #     -0.6
  #   else if (n_ics == 10)
  #     -0.62
  #   else
  #     -0.8
  # }
  #
  # lwd <- 10
  #
  # q_groups <- sort(unique(mix_df$questionnaire))
  #
  # for (i in 1:length(q_groups)) {
  #   q_dat <- mix_df %>%
  #     filter(questionnaire == q_groups[i]) %>%
  #     summarize(
  #       min_var_code = min(variable_code),
  #       max_var_code = max(variable_code)
  #     )
  #
  #   g <- g +
  #     ann(linesGrob(gp = gpar(col = cols[i], lwd = lwd)),
  #         xmin = ann_x, xmax = ann_x,
  #         ymin = q_dat$min_var_code[[1]], ymax = q_dat$max_var_code[[1]],
  #         data = pan_left_dat)
  # }

  # Code to override clipping
  gt <- ggplotGrob(g)
  gt$layout[grepl("panel", gt$layout$name), ]$clip <- "off"

  # Draw the plot
  grid.newpage()
  grid.draw(gt)

  if (!is.null(save_path)) {
    pdf(save_path, width = width, height = height)

    grid.newpage()
    grid.draw(gt)

    dev.off()
  }
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

mark_general_comp <- function(icas, first_general_idx, out_path) {
  general_idx <- vector("numeric", length(icas))

  general_comp <- icas[[1]]$A[first_general_idx,]

  for (i in 1:length(icas)) {
    A <- icas[[i]]$A

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

    # Determine which component of this solution most strongly correlates with
    # general_comp. First, glue general_comp to A, then calculate correlation
    # matrix.
    cors <- cor(t(rbind(general_comp, A)))
    # Correlations with general_comp are in first row/column. Discard very first
    # value, as that is correlation of general_comp with itself (i.e., 1).
    cors <- cors[2:(nrow(A)+1), 1]
    cor_idx <- which(abs(cors) == max(abs(cors)))

    fancy_mix_fig_sens(mix_df, cor_idx, lims = c(-0.6, 0.6))

    general_idx[i] <- readline(str_glue("ICA {i}: "))
  }

  saveRDS(general_idx, out_path)

  general_idx
}

mark_general_comp(icas_cent, first_general_idx = 5,
                  out_path = file.path(dat_dir, "general_idx_cent.rds"))
