# Load packages
library(ggplot2)
library(ggpubr)
library(purrr)
library(stringr)
library(shiny)
library(shinyMatrix)

# Create user interface
ui <- fluidPage(
  titlePanel(
    headerPanel("Linear Transformations")
  ),
  sidebarLayout(
    sidebarPanel(
      radioButtons(
        "dimension", 
        label = "Choose number of dimensions", 
        choices = c("2D", "3D"), 
        selected = 2
      ),
      matrixInput(
        "transform_mat",
        value = matrix(c(1, 0, 0, 1), nrow = 2, ncol = 2, byrow = FALSE),
        rows = list(names = FALSE),
        cols = list(names = FALSE),
        class = "numeric",
        copy = TRUE,
        paste = TRUE
      ),
      matrixInput(
        "transform_vec",
        value = matrix(c(1, 1), nrow = 2, ncol = 1, byrow = FALSE),
        rows = list(names = FALSE),
        cols = list(names = FALSE),
        class = "numeric",
        copy = TRUE,
        paste = TRUE
      )
    ),
    mainPanel(
      plotlyOutput("transform_plot")
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
    vec_options <- list(
      "2D" = matrix(c(1, 1), nrow = 2, ncol = 1, byrow = FALSE),
      "3D" = matrix(c(1, 1, 1), nrow = 3, ncol = 1, byrow = FALSE)
    )
    updateMatrixInput(
      session, 
      "transform_mat", 
      value = mat_options[[input$dimension]]
    )
    updateMatrixInput(
      session, 
      "transform_vec",
      value = vec_options[[input$dimension]]
    )
  })
  output$transform_plot <- renderPlotly({
    A <- input$transform_mat
    v <- input$transform_vec
    b1 <- matrix(c(1, 0), nrow = 2, ncol = 1, byrow = TRUE)
    b2 <- matrix(c(0, 1), nrow = 2, ncol = 1, byrow = TRUE)
    u <- A %*% v
    b1_tr <- A %*% b1
    b2_tr <- A %*% b2
    iterable <- list(
      input = list(b1, b2, v),
      output = list(b1_tr, b2_tr, u)
    )
    plots <- map2(iterable, names(iterable), function(l, title) {
      df <- data.frame(
        x = c(l[[1]][1, 1], l[[2]][1, 1], l[[3]][1, 1]), 
        y = c(l[[1]][2, 1], l[[2]][2, 1], l[[3]][2, 1]),
        type = c("basis", "basis", "vector")
      )
      p <- df %>% 
        plot_ly(x = ~x, y = ~y, color = ~type) %>% 
        add_segments(x = 0, y = 0, xend = ~x, yend = ~y, colors = c("black", "red")) %>%
        layout(title = str_to_title(title))
      p
    })
    subplot(
      plots$input, 
      plots$output, 
      nrows = 1, 
      shareX = TRUE, shareY = TRUE
    )
  })
}

# Run shiny application
shinyApp(ui, server)
