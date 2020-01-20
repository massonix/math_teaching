# Install packages if not done yet
if (!requireNamespace("plotly", quietly = TRUE))
  install.packages("plotly")
if (!requireNamespace("purrr", quietly = TRUE))
  install.packages("purrr")
if (!requireNamespace("dplyr", quietly = TRUE))
  install.packages("dplyr")
if (!requireNamespace("stringr", quietly = TRUE))
  install.packages("stringr")
if (!requireNamespace("shiny", quietly = TRUE))
  install.packages("shiny")
if (!requireNamespace("shinythemes", quietly = TRUE))
  install.packages("shinythemes")
if (!requireNamespace("shinyMatrix", quietly = TRUE))
  install.packages("shinyMatrix")

# Load packages
library(plotly)
library(purrr)
library(dplyr)
library(stringr)
library(shiny)
library(shinythemes)
library(shinyMatrix)
library(xtable)

# Source script
setwd(dir = dirname(rstudioapi::getSourceEditorContext()$path))
source("utils.R")

# Create user interface
ui <- fluidPage(
  theme = shinytheme("cerulean"),
  titlePanel(
    headerPanel("Linear Transformations")
  ),
  withMathJax(),
  sidebarLayout(
    sidebarPanel(
      radioButtons(
        "dimension", 
        label = "Choose number of dimensions", 
        choices = c("2D", "3D"), 
        selected = "2D"
      ),
      matrixInput(
        "transform_mat",
        label = "Input the transformation matrix",
        value = matrix(c(1, 0, 0, 1), nrow = 2, ncol = 2, byrow = FALSE),
        rows = list(names = FALSE),
        cols = list(names = FALSE),
        class = "numeric",
        copy = TRUE,
        paste = TRUE
      )
    ),
    mainPanel(
      tabsetPanel(
        type = "tabs",
        tabPanel("Plot",  plotlyOutput("plot")),
        tabPanel("Null Space", uiOutput("null_space")),
        tabPanel("Determinant", uiOutput("determinant")),
        tabPanel("Inverse Matrix", uiOutput("inverse_matrix"))
      )
    )
  )
)

# Define server function
server <- function(input, output, session) {
  observeEvent(input$dimension, {
    mat_options <- list(
      "2D" = matrix(c(1, 0, 0, 1), nrow = 2, ncol = 2, byrow = FALSE),
      "3D" = matrix(c(1, 0, 0, 0, 1, 0, 0, 0, 1), nrow = 3, ncol = 3, byrow = FALSE)
    )
    updateMatrixInput(
      session, 
      "transform_mat", 
      value = mat_options[[input$dimension]]
    )
  })
  output$plot <- renderPlotly({
    if (input$dimension == "2D") {
      plots <- plot_2d(input$transform_mat)
    } else {
      plots <- plot_3d(input$transform_mat)
    }
    plots
  })
  output$determinant <- renderUI({
    withMathJax(
      helpText(write_determinant(input$transform_mat, dimension = input$dimension))
    )
  })
}

# Run shiny application
shinyApp(ui, server)

