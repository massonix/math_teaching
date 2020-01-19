###############################################################################
#############Functions Shiny app on linear transformations#####################
###############################################################################

plot_2d <- function(transformation_matrix) {
  input_matrix <- matrix(
    c(c(0, 1, 1, 0), c(0, 0, 1, 1)),
    nrow = 2, 
    ncol = 4,
    byrow = TRUE
  )
  output_matrix <- transformation_matrix %*% input_matrix
  input_df <- as.data.frame(t(input_matrix))
  colnames(input_df) <- c("x", "y")
  output_df <- as.data.frame(t(output_matrix))
  colnames(output_df) <- c("x", "y")
  df <- bind_rows(list(input = input_df, output = output_df), .id = "status")
  df$color <- rep(c("red", "green", "blue", "yellow"), 2)
  min_coordinate <- min(min(input_matrix), min(output_matrix))
  max_coordinate <- max(max(input_matrix), max(output_matrix))
  if (abs(min_coordinate) > abs(max_coordinate)) {
    max_coordinate <- abs(min_coordinate)
  } else {
    min_coordinate <- -1 * max_coordinate
  }
  axis_lim <- list(nticks = 6, range = c(min_coordinate - 1, max_coordinate + 1))
  cols <- c("red", "green", "blue", "yellow")
  plot_list <- purrr::map(c("input", "output"), function(type) {
    p <- df %>% 
      filter(status == type) %>% 
      plot_ly(x = ~x, y = ~y) %>%
      add_trace(
        type = "scatter",
        fill = "toself",
        fillcolor = "gray",
        opacity = 0.2,
        hoveron = "points+fills",
        line = list(
          color = "black"
        ),
        text = "Points + Fills",
        hoverinfo = "text"
      ) %>% 
      add_markers(color = ~color, marker = list(color = cols, size = 10) ) %>% 
      layout(
        showlegend = FALSE, 
        xaxis = axis_lim, 
        yaxis = axis_lim,
        annotations = list(
          x = 0, 
          y = max_coordinate + 1, 
          text = str_to_title(type), 
          font = list(size = 20, face = "bold"),
          showarrow = FALSE
        )
      )
    p
  })
  subplot(
    plot_list[[1]], 
    plot_list[[2]], 
    nrows = 1, 
    margin = 0.05, 
    titleX = TRUE, 
    titleY = TRUE
  )
}


plot_3d <- function(transformation_matrix) {
  # Input matrix: each column vector is a point in space of a cube
  input_matrix <- matrix(
    c(c(0, 0, 1, 1, 0, 0, 1, 1), c(0, 1, 1, 0, 0, 1, 1, 0), c(0, 0, 0, 0, 1, 1, 1, 1)),
    nrow = 3, 
    ncol = 8,
    byrow = TRUE
  )
  output_matrix <- transformation_matrix %*% input_matrix
  input_df <- as.data.frame(t(input_matrix))
  colnames(input_df) <- c("x", "y", "z")
  output_df <- as.data.frame(t(output_matrix))
  colnames(output_df) <- c("x", "y", "z")
  df <- bind_rows(list(input = input_df, output = output_df), .id = "status")
  min_coordinate <- min(min(input_matrix), min(output_matrix))
  max_coordinate <- max(max(input_matrix), max(output_matrix))
  if (abs(min_coordinate) > abs(max_coordinate)) {
    max_coordinate <- abs(min_coordinate)
  } else {
    min_coordinate <- -1 * max_coordinate
  }
  axis_lim <- list(nticks = 6, range = c(min_coordinate, max_coordinate))
  plot_list <- purrr::map2(c("input", "output"), c("scene1", "scene2"), function(type, scene) {
    if (type == "output" & qr(transformation_matrix)$rank == 1) {
      index <- which(colSums(transformation_matrix != 0) != 0)[1]
      line_vector <- transformation_matrix[, index]
      scalars <- seq(0, 1, by = 0.01)
      points <- map(scalars, ~ .x * line_vector)
      points <- t(as.data.frame(points))
      rownames(points) <- NULL
      colnames(points) <- c("x", "y", "z")
      points <- as.data.frame(points)
      p_output <- points %>%
        plot_ly(x = ~x, y = ~y, z = ~z, scene = scene) %>% 
        add_lines()
      return(p_output)
    } else {
      p <- df %>% 
        filter(status == type) %>% 
        plot_ly(x = ~x, y = ~y, z = ~z, scene = scene) %>% 
        add_mesh(
          i = c(7, 0, 0, 0, 4, 4, 6, 6, 4, 0, 3, 2),
          j = c(3, 4, 1, 2, 5, 6, 5, 2, 0, 1, 6, 3),
          k = c(0, 7, 2, 3, 6, 7, 1, 1, 5, 5, 7, 6),
          intensity = seq(0, 1, length = 8),
          color = seq(0, 1, length = 8),
          colors = colorRamp(rainbow(8)),
          showscale = FALSE
        )
      if (type == "input") {
        p %>% 
          layout(scene = list(xaxis = axis_lim, yaxis = axis_lim, zaxis = axis_lim))
      } else {
        p %>% 
          layout(scene2 = list(xaxis = axis_lim, yaxis = axis_lim, zaxis = axis_lim))
      }
    }
  })
  names(plot_list) <- c("input", "output")
  subplot(plot_list$input, plot_list$output, margin = 0.5) %>% 
    layout(
      scene = list(domain = list(x = 0)),
      scene2 = list(domain = list(x = 0.5)),
      showlegend = FALSE
    )
}

matrix2latex <- function(matr) {
  
  printmrow <- function(x) {
    
    cat(cat(x, sep = " & "), "\\\\ \n")
  }
  
  cat("\\begin{bmatrix}","\n")
  body <- apply(matr, 1, printmrow)
  cat("\\end{bmatrix}")
}


matrix2latex <- function(x, digits=NULL, ...) {
  default_args = list(include.colnames=FALSE, only.contents=TRUE,
                      include.rownames=FALSE, hline.after=NULL, comment=FALSE,
                      print.results=FALSE)
  passed_args = list(...)
  calling_args = c(list(x=xtable(x, digits=digits)),
                   c(passed_args,
                     default_args[setdiff(names(default_args), names(passed_args))]))
  cat("$$\\begin{bmatrix}\n",
      do.call(print.xtable, calling_args),
      "\\$$end{bmatrix}\n")
}