# Install packages if not done yet
if (!requireNamespace("ggplot2", quietly = TRUE))
  install.packages("ggplot2")
if (!requireNamespace("purrr", quietly = TRUE))
  install.packages("purrr")
if (!requireNamespace("stringr", quietly = TRUE))
  install.packages("stringr")
if (!requireNamespace("ggpubr", quietly = TRUE))
  install.packages("ggpubr")
if (!requireNamespace("shiny", quietly = TRUE))
  install.packages("shiny")
if (!requireNamespace("shinyMatrix", quietly = TRUE))
  install.packages("shinyMatrix")

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
      matrixInput(
        "transform_vec",
        value = matrix(c(1, 1), nrow = 2, ncol = 1, byrow = FALSE),
        rows = list(names = FALSE),
        cols = list(names = FALSE),
        class = "numeric",
        copy = TRUE,
        paste = TRUE
      ),
      matrixInput(
        "transform_mat",
        value = matrix(c(1, 0, 0, 1), nrow = 2, ncol = 2, byrow = FALSE),
        rows = list(names = FALSE),
        cols = list(names = FALSE),
        class = "numeric",
        copy = TRUE,
        paste = TRUE
      )
  ),
    mainPanel(
      plotOutput("transform_plot")
    )
  )
)

# Define server function
server <- function(input, output, session) {
  output$transform_plot <- renderPlot({
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
    # max_x <- max(map_dbl(c(iterable$input, iterable$output), 1)) + 1
    # max_y <- max(map_dbl(c(iterable$input, iterable$output), 2)) + 1
    # breaks_x <- round(seq(from = -max_x, to = max_x, length.out = 10))
    # breaks_y <- round(seq(from = -max_y, to = max_y, length.out = 10))
    plots <- map2(iterable, names(iterable), function(l, title) {
      df <- data.frame(
        x = c(l[[1]][1, 1], l[[2]][1, 1], l[[3]][1, 1]), 
        y = c(l[[1]][2, 1], l[[2]][2, 1], l[[3]][2, 1]),
        type = c("basis", "basis", "vector")
      )
      p <- ggplot(df) +
        geom_segment(aes(x = 0, y = 0, xend = x, yend = y, color = type), 
                     arrow = arrow(length = unit(0.5, "cm"))) +
        scale_x_continuous(limits = c(-5, 5), breaks = seq(from = -5, to = 5, by = 1)) +
        scale_y_continuous(limits = c(-5, 5), breaks = seq(from = -5, to = 5, by = 1)) +
        scale_color_manual("", values = c("black", "red")) +
        ggtitle(str_to_title(title)) +
        theme_minimal() +
        theme(plot.title = element_text(size = 14, hjust = 0.5, face = "bold"))
      p
    })
    ggarrange(
      plotlist = plots, 
      ncol = 2, 
      common.legend = TRUE, 
      hjust = -4.5,
      vjust = 0.5
    )
  })
}

# Run shiny application
shinyApp(ui, server)