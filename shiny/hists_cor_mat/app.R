#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

### Load dependencies

local <- TRUE

wd <- ""

if (!local) {
  # Cluster environment
  wd <- "~/Documents/Autocorrelation"
  # Base directory for the data set we will create
  dat_dir <- "/project_cephfs/3022017.02/projects/lorkno/data"
} else {
  wd <- "~/HPC/Documents/Autocorrelation"
  # Base directory for the data set we will create
  dat_dir <- "~/HPC_project/data"
}

# For some reason this sets the pwd only for the current scope, which means it 
# does not affect anything outside an if block if you set it there.
# So that's why it's here instead of above.
setwd(wd) 

source("src/load_dependencies.R")

library(shiny)

### Set up data

load(file.path(dat_dir, "subject_mats.rda"))

ls <- load_subject_key_data(dat_dir, load_kp = FALSE)
subjects <- ls$subjects

n_sub <- length(subjects)

subs_cor_mat <- vector("list", n_sub)
subs_acfs <- vector("list", n_sub)

for (i in 1:n_sub) {
  subs_cor_mat[[i]] <- cor(sapply(subject_mats[[i]], as.vector))
  # g1 <- ggplot(melt(cors), aes(X1, X2)) +
  #   geom_raster(aes(fill = value)) +
  #   scale_y_reverse() +
  #   ggtitle(str_glue("Subject {subjects[i]}"))
  #
  # df <- data.frame(mean_cor = colMeans(cors), X1 = 1:ncol(cors))
  # g2 <- ggplot(df, aes(X1, mean_cor)) +
  #   geom_line() +
  #   geom_point() +
  #   ylim(0, 1) +
  #   scale_x_continuous(breaks = seq(0, ncol(cors), 10),
  #                      minor_breaks = 1:ncol(cors)) +
  #   ylab("Mean correlation")

  subs_acfs[[i]] <- apply(subs_cor_mat[[i]], 1, acf, lag.max = 300, plot = FALSE)


  # df <- melt(t(sapply(acfs, function(x) x$acf)))
  # g3 <- ggplot(df, aes(X2, value)) +
  #   geom_point(alpha = 0.2) +
  #   scale_x_continuous(breaks = seq(0, ncol(cors), 10),
  #                      minor_breaks = 1:ncol(cors)) +
  #   xlab("X1") +
  #   ylab("Autocorrelation")
  #
  # g <- ggarrange(g1, g2, g3, nrow = 3, common.legend = TRUE)
  #
  # print(g)
  #
  # ggsave(str_glue("images/autocorrelation/correlations/sub-{subjects[i]}_cor-mat_autocor.png"))
}

### Shiny app

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Subject histograms and correlation matrix"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("subject",
                        "Subject number:",
                        min = 1,
                        max = n_sub,
                        value = 1),
            sliderInput("day",
                        "Day:",
                        min = 1,
                        max = 1,
                        value = 1,
                        step = 1)
        ),

        # Show a plot of the generated distribution
        mainPanel(
          plotOutput("corMat"),
          plotOutput("hist")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  max_days <- reactive({
    length(subject_mats[[input$subject]])
  })

  observe({
    updateSliderInput(inputId = "day", 
                      value = min(input$day, max_days()),
                      max = max_days())
  })
  
  output$corMat <- renderPlot({
    mat <- subs_cor_mat[[input$subject]]
    
    ggplot(melt(mat), aes(X2, X1)) +
      geom_raster(aes(fill = value)) +
      scale_x_continuous(breaks = seq(0, ncol(mat), by = 10)) +
      scale_y_reverse() +
      ggtitle(str_glue("Subject {subjects[input$subject]}"))
  })
  
  output$hist <- renderPlot({
    ggplot(melt(subject_mats[[input$subject]][[input$day]]), aes(X1, X2)) +
      geom_raster(aes(fill = value)) +
      xlab("distanceFromPrevious bin") +
      ylab("IKD bin")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
