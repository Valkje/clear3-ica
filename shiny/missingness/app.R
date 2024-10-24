#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Options:
# - Removal threshold BiAffect
# - Removal threshold self-report
# - Inclusion columns

# View:
# - Longitudinal missing data patterns of present subjects
# - TableOne output

# To-do's:
# - Add option to view missing patterns for single subject

### Load dependencies

local <- TRUE

wd <- ""

if (!local) {
  # Cluster environment
  wd <- "~/Documents/Autocorrelation"
  # Base directory for the data set we will create
  dat_dir <- "/project_cephfs/3022017.02/projects/lorkno/data"
} else {
  wd <- "/Volumes/home/preclineu/lorkno/Documents/clear3-ica"
  # Base directory for the data set we will create
  dat_dir <- "/Volumes/project/3022000.05/projects/lorkno/data"
}

# Won't work in non-local environment
man_img_dir <- file.path("~/Documents/Writing/Papers/Paper 1/images")

# For some reason this sets the pwd only for the current scope, which means it
# does not affect anything outside an if block if you set it there.
# So that's why it's here instead of above.
setwd(wd)

library(reshape)
library(scales)
library(lubridate)
library(tidyverse)
library(hrbrthemes)
library(tableone)
library(ggpubr)
library(shiny)
library(DT)

### Set up data

load(file.path(dat_dir, "dat_reg_tdia_30_blocked_expanded.rda"))
dat_reg_trimmed <- readRDS(
  file.path(dat_dir,"dat_reg_trimmed_tdia_30_blocked_expanded.rds"))

dat_reg <- dat_reg %>%
  ungroup()

dat_cols <- colnames(dat_reg)
miss_col_idx <- which(dat_cols == "medianIKD"):length(dat_cols)
last_bi_idx <- which(dat_cols == "mean_accuracy")

bi_cols <- dat_cols[miss_col_idx[1]:last_bi_idx]
sr_cols <- dat_cols[(last_bi_idx+1):length(dat_cols)]
dat_cols <- dat_cols[miss_col_idx]

choices = as.list(dat_cols)

# na_pat_proto <- dat_reg %>%
#   mutate(across(medianIKD:n_stressors, is.na)) %>%
#   pivot_longer(medianIKD:n_stressors, names_to = "variable", values_to = "missing") %>%
#   mutate(
#     present = !missing, # For convenience
#     modality = case_when(
#       variable %in% bi_cols ~ "biaffect",
#       TRUE ~ "self_report"
#     )
#   )

order_opts <- c(
  "Subject ID" = "subject",
  "BiAffect missingness (increasing)" = "biaffect",
  "Self-report missingness (increasing)" = "self_report"
)

# Define UI for application
ui <- fluidPage(

  tags$head(
    tags$style(HTML("
      .shiny-input-container:not(.shiny-input-container-inline) {
        width: 100%;
      }
    "))
  ),

  # Application title
  titlePanel("CLEAR-3 missing data patterns"),

  hr(),

  selectInput("dataset", "Data set to use:",
              c(
                "Untrimmed" = "dat_reg",
                "Trimmed" = "dat_reg_trimmed"
              ),
              selected = "dat_reg_trimmed"),

  hr(),

  fluidRow(
    column(6,
           column(6,
                  sliderInput("thr_biaffect",
                              "Remove subjects with missing BiAffect data percentage above:",
                              min = 0,
                              max = 100,
                              value = 100),

                  sliderInput("thr_self_report",
                              "Remove subjects with missing self-report data percentage above:",
                              min = 0,
                              max = 100,
                              value = 100)
           ),

           column(6,
                  h5("Columns to incorporate in missingness calculation:"),
                  wellPanel(
                    checkboxGroupInput("sel_cols",
                                       NULL,
                                       choices = choices,
                                       # We want to exclude TDIA measures by default for now
                                       selected = choices[! choices %in% c("partition_idx", "mean_accuracy")],
                                       inline = FALSE),
                    style = "overflow: auto; height: 200px"
                  ),
           ),

           column(12,
                  numericInput("thr_keypresses",
                               "Minimum number of BiAffect key presses per day:",
                               value = 750, min = 0, max = 1000)),

           column(12,
                  verbatimTextOutput("text"))
    ),

    column(6,
           tabsetPanel(
             tabPanel("Statistics per variable", DTOutput("table_one")),
             tabPanel("Missing data proportion per modality", DTOutput("miss_props"))
           )
    )
  ),

  hr(),
  fluidRow(

    column(12,
           h4("Distribution of missing data proportions across subjects"),
           tabsetPanel(
             tabPanel("Across all variables", plotOutput("violin_across_vars")),
             tabPanel("Per variable", plotOutput("violin_per_var")),
             type = "tabs"
           )
    ),

  ),

  hr(),
  h4("Missing data patterns"),
  fluidRow(
    column(3,
      selectInput("order_by",
                  "Order by:",
                  order_opts),
    ),
    column(3,
      numericInput("thr_contiguity",
                   "Contiguity threshold:",
                    value = 0, min = 0, max = 1000) # We could make the max adaptive
    ),
    column(3,
      selectInput("present_op",
                  "Mark as present if any or all of the variables are present?",
                  c(
                    "Any" = "any",
                    "All" = "all"
                  ),
                  selected = "all")
    ),
    column(3,
      selectInput("miss_pat_sub",
                  "Subject to view:",
                  c("All" = "all", sort(dat_reg$subject)))
    )
  ),

  fluidRow(
    column(3,
      checkboxInput("decoupled",
                    "Decouple BiAffect selection from contiguity condition",
                    value = TRUE)
    )
  ),
  fluidRow(
    column(3,
      wellPanel(
        actionButton("save_fig", "Save missing pattern figure", style = "width: 100%;")
      )
    ),
    column(3,
      wellPanel(
        actionButton("save", "Save selection to file", style = "width: 100%;")
      )
    )
  ),


  tabsetPanel(
    tabPanel("Faceted on modality", plotOutput("miss_pat", height = "1000px")),
    tabPanel("Faceted on subject", plotOutput("miss_pat_per_sub", height = "1000px")),
    type = "tabs"
  )

)

server <- function(input, output) {

  dat <- reactive({
    if (input$dataset == "dat_reg") {
      return(dat_reg)
    }

    dat_reg_trimmed
  })

  na_pat_proto <- reactive({
    dat() %>%
      # A slightly hacky way to mark certain BiAffect entries as missing if they
      # don't contain enough key presses
      mutate(totalKeyPresses = case_when(
        totalKeyPresses < input$thr_keypresses ~ NA_integer_,
        TRUE ~ totalKeyPresses
      )) %>%
      mutate(across(medianIKD:mastery, is.na)) %>%
      pivot_longer(medianIKD:mastery, names_to = "variable", values_to = "missing") %>%
      mutate(
        present = !missing, # For convenience
        modality = case_when(
          variable %in% bi_cols ~ "biaffect",
          TRUE ~ "self_report"
        )
      )
  })

  na_pat_flt_cols <- reactive({
    na_pat_proto() %>%
      filter(variable %in% input$sel_cols)
  })

  miss_props <- reactive({
    na_pat_flt_cols() %>%
      group_by(subject, variable) %>%
      summarize(
        missing = mean(missing),
        modality = modality[1]
      ) %>%
      group_by(subject, modality) %>%
      summarize(missing = mean(missing)) %>%
      pivot_wider(id_cols = subject, 
                  names_from = modality, values_from = missing)
  }) %>%
    bindCache(input$dataset, input$sel_cols)

  thr_bi <- reactive(input$thr_biaffect / 100)
  thr_sr <- reactive(input$thr_self_report / 100)

  bad_subs <- reactive({
    filter(miss_props(),
           biaffect > thr_bi() | self_report > thr_sr())$subject
  })

  na_pat_flt_subs <- reactive({
    na_pat_flt_cols() %>%
      filter(!(subject %in% bad_subs()))
  })

  # Note: Returns a function
  present_op <- reactive({
    if (input$present_op == "all") {
      return(all)
    }

    return(any)
  })

  na_pat_collapsed <- reactive({
    na_pat_flt <- na_pat_flt_subs()
    if (input$miss_pat_sub != "all") {
      na_pat_flt <- na_pat_flt %>%
        filter(subject == input$miss_pat_sub)
    }

    na_pat_col_unordered <- na_pat_flt %>%
      group_by(subject, date, modality) %>%
      summarize(present = present_op()(present)) %>%
      group_by(subject, modality) %>% # Mark contiguous blocks
      arrange(date, .by_group = TRUE) %>%
      mutate(
        # TRUE if transitioning from missing to present or diff(date) > 1
        cuts = c(TRUE, diff(present) == 1 | diff(date) > 1),
        # cumsum calculates group number, mask by present
        block = na_if(
          present * cumsum(cuts),
          0
        )
      ) %>%
      group_by(subject, modality, block) %>%
      mutate(good_block = case_when(
        input$decoupled & modality == "biaffect" ~ !is.na(block),
        TRUE ~ !is.na(block) & n() >= input$thr_contiguity
      )) %>%
      ungroup() %>%
      # Only include biaffect within good self-report blocks
      select(subject, date, modality, present, good_block) %>%
      pivot_wider(id_cols = c(subject, date), names_from = modality,
                  values_from = c(present, good_block),
                  names_sep = ".") %>%
      mutate(good_block.biaffect = good_block.biaffect & good_block.self_report) %>%
      pivot_longer(!c(subject, date),
                   names_to = c(".value", "modality"),
                   names_pattern = "(.+)\\.(.+)")

    # print(na_pat_col_unordered)

    ordered_sub_lvls <- na_pat_col_unordered %>%
      group_by(subject, modality) %>%
      summarize(present = mean(present)) %>%
      pivot_wider(id_cols = subject, 
                  names_from = "modality", values_from = "present") %>%
      arrange(.data[[input$order_by]]) %>%
      select(subject)

    na_pat_col_unordered %>%
      mutate(subject = factor(subject,
                              levels = ordered_sub_lvls$subject,
                              ordered = TRUE))
  })

  # Update missing pattern subject view
  observe({
    subs <- sort(as.character(unique(na_pat_flt_subs()$subject)))

    # Don't change selected subject, unless it is no longer in na_pat_collapsed
    sel_sub <- input$miss_pat_sub
    if (!(sel_sub %in% subs)) {
      sel_sub <- "all"
    }

    updateSelectInput(
      inputId = "miss_pat_sub",
      selected = sel_sub,
      choices = c("All" = "all", subs)
    )
  })

  ######### Text and tables #########

  output$text <- renderText({
    n_sub <- n_distinct(dat()$subject)

    n_bad_bi <- nrow(filter(miss_props(), biaffect > thr_bi()))
    n_bad_sr <- nrow(filter(miss_props(), self_report > thr_sr()))
    n_bad_both <- length(bad_subs())

    str_glue(
      "Number of subjects in unfiltered data: {n_sub}.",
      "Number of subjects with >{input$thr_biaffect}% BiAffect data missing: {n_bad_bi}.",
      "Number of subjects with >{input$thr_self_report}% self-report data missing: {n_bad_sr}.",
      "Number of subjects actually excluded: {n_bad_both}.",
      .sep = "\n"
    )
  })

  output$miss_props <- renderDT({
    miss_props()
  })

  output$table_one <- renderDT({
    dat() %>%
      filter(!(subject %in% bad_subs())) %>%
      pivot_longer(where(is.numeric), names_to = "variable") %>%
      group_by(variable) %>%
      summarize(
        n = n(),
        miss = sum(is.na(value)),
        p.miss = round(mean(is.na(value)), 2),
        mean = round(mean(value, na.rm = TRUE), 2),
        sd = round(sd(value, na.rm = TRUE), 2)
      ) %>%
      mutate(modality = case_when(
        variable %in% bi_cols ~ "biaffect",
        TRUE ~ "self-report"
      )) %>%
      arrange(modality)
  })

  ######### Plots #########

  output$histogram <- renderPlot({
    miss_props_long <- miss_props() %>%
      filter(!(subject %in% bad_subs())) %>%
      pivot_longer(c(biaffect, self_report), names_to = "modality", values_to = "miss_prop")

    ggplot(miss_props_long) +
      geom_histogram(aes(miss_prop, fill = modality),
                     position = "identity", alpha = 0.5) +
      xlab("Average proportion of missing data") +
      ylab("Number of subjects") +
      scale_fill_discrete(labels = c("BiAffect", "Self-report"),
                          guide = guide_legend("Modality")) +
      theme_minimal() +
      theme(text = element_text(size = 14))
  })

  output$violin_across_vars <- renderPlot({
    miss_props_long <- na_pat_flt_subs() %>%
      group_by(subject, variable) %>%
      summarize(
        missing = mean(missing),
        modality = modality[1]
      ) %>%
      group_by(subject, modality) %>%
      summarize(missing = mean(missing))

    ggplot(miss_props_long, aes(modality, missing, fill = modality)) +
      geom_violin() +
      geom_jitter(width = 0.02, alpha = 0.5) +
      xlab("Modality") +
      ylab("Proportion of missing data") +
      scale_fill_discrete(labels = c("BiAffect", "Self-report"),
                          guide = guide_legend("Modality:")) +
      theme(text = element_text(size = 14),
            legend.position = "top")
  })

  output$violin_per_var <- renderPlot({
    miss_props_var <- na_pat_flt_subs() %>%
      group_by(variable, subject) %>%
      summarize(
        prop_miss = mean(missing),
        modality = modality[1]
      )

    ggplot(miss_props_var, aes(variable, prop_miss)) +
      geom_violin(aes(fill = modality)) +
      geom_jitter(width = 0.2, alpha = 0.2) +
      xlab("Variable") +
      ylab("Proportion of missing data") +
      scale_fill_discrete(labels = c("BiAffect", "Self-report"),
                          guide = guide_legend("Modality:")) +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
            text = element_text(size = 14),
            legend.position = "top")
  })

  miss_pat_plot <- reactive({
    na_pat_coll <- na_pat_collapsed()

    counts <- na_pat_coll %>%
      group_by(subject, modality) %>%
      summarize(
        n = sum(good_block),
        max_date = max(date)
      )

    n_sub <- n_distinct(counts$subject)

    facet_labels <- c(
      "biaffect" = "BiAffect",
      "self_report" = "Self-report"
    )

    min_date <- min(na_pat_coll$date) - months(1)
    # We need 0.5 month extra to prevent the tick mark from running off the
    # margin
    max_date <- max(na_pat_coll$date) + days(45)

    ggplot(na_pat_coll, aes(date, subject)) +
      geom_tile(aes(fill = present)) +
      geom_point(aes(color = good_block), size = 0.1) +
      scale_fill_manual(values = c("darkgrey", "steelblue"),
                        labels = c("Missing", "Present"), name = NULL) +
      scale_color_discrete(labels = c("Excluded", "Included"), name = NULL) +
      xlim(min_date, max_date) +
      facet_wrap(~ modality, nrow = 1, labeller = as_labeller(facet_labels)) +
      xlab("Date") +
      ylab("Participant number") +
      theme_minimal() +
      theme(text = element_text(size = 18),
            axis.text.y = element_text(size = 10),
            legend.position = "top") +
      guides(color = guide_legend(override.aes = list(size = 5)))
  })

  output$miss_pat <- renderPlot({
    na_pat_coll <- na_pat_collapsed()

    counts <- na_pat_coll %>%
      group_by(subject, modality) %>%
      summarize(
        n = sum(good_block),
        max_date = max(date)
      )

    n_sub <- n_distinct(counts$subject)
    min_date <- min(na_pat_coll$date)

    g <- miss_pat_plot() +
      geom_label(x = min_date,
                 y = levels(counts$subject)[n_sub],
                 hjust = "left",
                 vjust = "top",
                 aes(label = str_glue("Total number of observations: {n}")),
                 data = counts %>%
                   group_by(modality) %>%
                   summarize(n = sum(n))) +
      geom_text(aes(y = subject, label = n, x = max(max_date) + months(1)),
                hjust = "right",
                data = counts)

    if (n_sub > 1) {
      g <- g +
        geom_label(x = min_date,
                   y = levels(counts$subject)[n_sub-3],
                   hjust = "left",
                   vjust = "top",
                   aes(label = str_glue("Average number of observations per subject (SD): {round(mean, 2)} ({round(sd, 2)})")),
                   data = counts %>%
                     group_by(modality) %>%
                     summarize(mean = mean(n), sd = sd(n)))
    }

    g
  })

  output$miss_pat_per_sub <- renderPlot({
    na_pat_col <- na_pat_collapsed()

    # A bit awkward, but not much I can do about it
    if (input$order_by != "subject") {
      na_pat_col$subject <- fct_rev(na_pat_col$subject)
    }

    ggplot(na_pat_col, aes(date, modality)) +
      geom_tile(aes(fill = present)) +
      geom_point(aes(color = good_block), size = 0.1) +
      scale_fill_manual(values = c("darkgrey", "steelblue"),
                        labels = c("Missing", "Present"), name = NULL) +
      scale_color_discrete(labels = c("Excluded", "Included"), name = NULL) +
      theme_minimal() +
      xlab("Date") +
      ylab("Modality") +
      facet_wrap(~ subject, scales = "free_x") +
      theme(text = element_text(size = 12),
            axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
            legend.position = "top",
            legend.text = element_text(size = 16))
  })

  ######### Actions #########

  observeEvent(input$save_fig, {
    na_pat_coll <- na_pat_collapsed()

    n_sub <- n_distinct(na_pat_coll$subject)
    sub_id_labels <- sapply(1:n_sub, function(i) if ((i - 1) %% 5 == 0) i - 1 else "")

    g <- miss_pat_plot() +
      scale_y_discrete(labels = sub_id_labels) +
      theme(
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14), # Overrides earlier specification
        strip.text.x = element_text(size = 16)
      )

    ggsave(file.path(man_img_dir, "miss_pat_fragmented_no-tdia.pdf"), g,
           width = 12, height = 12)

    ggsave(file.path(man_img_dir, "miss_pat_fragmented_no-tdia.png"), g,
           width = 12, height = 12)

    message("Figure saved successfully.")
  })

  observeEvent(input$save, {
    na_pat_col <- na_pat_collapsed() %>%
      select(subject, date, modality, good_block)

    # Note: We leave this in long format, makes modality selection easier in
    # downstream analyses
    dat <- dat() %>%
      pivot_longer(medianIKD:mastery, names_to = "variable") %>%
      mutate(
        modality = case_when(
          variable %in% bi_cols ~ "biaffect",
          TRUE ~ "self_report"
        )
      ) %>%
      filter(variable %in% input$sel_cols, !(subject %in% bad_subs())) %>%
      inner_join(na_pat_col, c("subject", "date", "modality")) %>%
      filter(good_block) %>%
      select(!good_block)

    saveRDS(dat, file.path(dat_dir, "dat_reg_fragmented_no-tdia.rds"))

    # Missingness markers might be useful later on
    saveRDS(na_pat_col, file.path(dat_dir, "missingness_fragmented_no-tdia.rds"))

    message("Data saved successfully.")
  })
}

# Run the application
shinyApp(ui = ui, server = server)
