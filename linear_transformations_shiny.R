# # Install packages if not done yet
# if (!requireNamespace("plotly", quietly = TRUE))
#   install.packages("plotly")
# if (!requireNamespace("purrr", quietly = TRUE))
#   install.packages("purrr")
# if (!requireNamespace("dplyr", quietly = TRUE))
#   install.packages("dplyr")
# if (!requireNamespace("stringr", quietly = TRUE))
#   install.packages("stringr")
# if (!requireNamespace("shiny", quietly = TRUE))
#   install.packages("shiny")
# if (!requireNamespace("shinythemes", quietly = TRUE))
#   install.packages("shinythemes")
# if (!requireNamespace("shinyMatrix", quietly = TRUE))
#   install.packages("shinyMatrix")

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
        tabPanel("Null Space", verbatimTextOutput("null_space"), plotlyOutput("null_space_plot")),
        tabPanel("Determinant", uiOutput("determinant")),
        tabPanel(
          "Gauss-Jordan", 
          tableOutput("gauss_matrix"), 
          tableOutput("gauss_identity"), 
          actionButton("rewind", "", icon = icon("fast-backward")),
          actionButton("play", "", icon = icon("play")),
          actionButton("advance", "", icon = icon("fast-forward"))
        )
      )
    )
  )
)


# Define server function
mat_list <- list(matrix(), matrix())
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
  output$null_space <- renderPrint({ 
    compute_null_space(input$transform_mat) 
  })
  output$null_space_plot <- renderPlotly({
    if (input$dimension == "2D") {
      plot_null_space_2d(input$transform_mat)
    } else {
      plot_null_space_3d(input$transform_mat)
    }
  })
  observeEvent(input$transform_mat, {
    mat_list <<- list(
      input$transform_mat,
      diag(nrow(input$transform_mat))
    )
    output$gauss_matrix <- renderTable({
      mat_list[[1]]
    }, colnames = FALSE)
    output$gauss_identity <- renderTable({
      mat_list[[2]]
    }, colnames = FALSE)
  })
  observeEvent(input$play, {
    if (nrow(input$transform_mat) == 2) {
      mat_list <<- apply_gauss_jordan_2d(mat_list[[1]], mat_list[[2]])
    } else if (nrow(input$transform_mat) == 3) {
      mat_list <<- apply_gauss_jordan_3d(mat_list[[1]], mat_list[[2]])
    }
    output$gauss_matrix <- renderTable({
      mat_list[[1]]
    }, colnames = FALSE)
    output$gauss_identity <- renderTable({
      mat_list[[2]]
    }, colnames = FALSE)
  })
  observeEvent(input$rewind, {
    mat_list <<- list(
      input$transform_mat,
      diag(nrow(input$transform_mat))
    )
    output$gauss_matrix <- renderTable({
      mat_list[[1]]
    }, colnames = FALSE)
    output$gauss_identity <- renderTable({
      mat_list[[2]]
    }, colnames = FALSE)
  })
  observeEvent(input$advance, {
    if (nrow(input$transform_mat) == 2) {
      while (any(mat_list[[1]] != apply_gauss_jordan_2d(mat_list[[1]], mat_list[[2]])[[1]])) {
        mat_list <<- apply_gauss_jordan_2d(mat_list[[1]], mat_list[[2]])
      }
    } else if (nrow(input$transform_mat) == 3) {
      while (any(mat_list[[1]] != apply_gauss_jordan_3d(mat_list[[1]], mat_list[[2]])[[1]])) {
        mat_list <<- apply_gauss_jordan_3d(mat_list[[1]], mat_list[[2]])
      }
    }
    output$gauss_matrix <- renderTable({
      mat_list[[1]]
    }, colnames = FALSE)
    output$gauss_identity <- renderTable({
      mat_list[[2]]
    }, colnames = FALSE)
  })
}

# Run shiny application
shinyApp(ui, server)
