# Load packages
library(shiny)
library(shinythemes)
library(shinyMatrix)
library(plotly)
library(matlib)
library(reshape2)

# Source utils
# setwd("~/Google Drive/single_cell/PhD/trainings/math_teaching/math_teaching/")
source("utils_projections_onto_subspaces.R")

# Create user interface
ui <- fluidPage(
  theme = shinytheme("cerulean"),
  titlePanel(
    headerPanel("Projections onto Subspaces")
  ),
  withMathJax(),
  sidebarLayout(
    sidebarPanel(
      matrixInput(
        "input_mat",
        label = "Matrix A",
        value = matrix(c(1, 0, 0, 0, 1, 0), nrow = 3, ncol = 2, byrow = FALSE),
        rows = list(names = FALSE),
        cols = list(names = FALSE),
        class = "numeric",
        copy = TRUE,
        paste = TRUE
      ),
      matrixInput(
        "input_vec",
        label = "Vector b",
        value = matrix(c(1, 0, 0), nrow = 3, ncol = 1, byrow = FALSE),
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
        tabPanel("Visualization",  plotlyOutput("plot"))
      )
    )
  )
)

# # Define inputs
# A <- matrix(c(1, 0, 1, 2, 2, 0), nrow = 3, ncol = 2)
# b <- c(4, 1, 4)
server <- function(input, output, session) {
  output$plot <- renderPlotly({
    # Find projection (p) of b on column space of A (C(A))
    p <- input$input_mat %*% inv(t(input$input_mat) %*% input$input_mat) %*% t(input$input_mat) %*% as.matrix(input$input_vec)
    
    # Compute C(A)
    col_space_df <- calculate_col_space(input$input_mat, input$input_vec, p)
    
    # Plot
    plot_projection(input$input_vec, p, col_space_df)
  })
}

# Run shiny application
shinyApp(ui, server)



# 
