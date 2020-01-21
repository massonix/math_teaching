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
  cols <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", 
            "#D55E00", "#CC79A7")
  df$color <- rep(cols, 2)
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
          colors = "gray",
          showscale = FALSE
        ) %>% 
        add_markers(color = ~color, marker = list(color = cols, size = 10))
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


matrix2latex <- function(mat) {
  header <- "\\begin{bmatrix}\n "
  latex_mat <- ""
  for (i in 1:nrow(mat)) {
    for (j in 1:ncol(mat)) {
      if (j != ncol(mat)) {
        latex_mat <- str_c(latex_mat, mat[i,j]," & ")
      } else {
        latex_mat <- str_c(latex_mat, mat[i,j], " \\\\ \n ")
      }
    }
  }
  ending <- "\\end{bmatrix}"
  string <- str_c(header, latex_mat, ending, sep = "")
  string
}

det2latex <- function(mat) {
  header <- "\\begin{vmatrix}\n "
  latex_mat <- ""
  for (i in 1:nrow(mat)) {
    for (j in 1:ncol(mat)) {
      if (j != ncol(mat)) {
        latex_mat <- str_c(latex_mat, mat[i,j]," & ")
      } else {
        latex_mat <- str_c(latex_mat, mat[i,j], " \\\\ \n ")
      }
    }
  }
  ending <- "\\end{vmatrix}"
  string <- str_c(header, latex_mat, ending, sep = "")
  string
}

write_determinant <- function(mat, dimension = "2D") {
  beginning <- "$$det(A)="
  det_latex <- det2latex(mat)
  if (dimension == "2D") {
    computation <- str_c(
      as.character(mat[1, 1]), 
      "\\cdot", 
      as.character(mat[2, 2]), 
      "-", 
      as.character(mat[1, 2]), 
      "\\cdot", 
      as.character(mat[2, 1]),
      sep = ""
    )
  } else {
    computation <- str_c(   
    as.character(mat[1, 1]), "\\cdot", as.character(mat[2, 2]), "\\cdot", as.character(mat[3, 3]), "+",
    as.character(mat[1, 2]), "\\cdot", as.character(mat[2, 3]), "\\cdot", as.character(mat[3, 1]), "+",
    as.character(mat[1, 3]), "\\cdot", as.character(mat[2, 1]), "\\cdot", as.character(mat[3, 2]), "-",
    as.character(mat[1, 3]), "\\cdot", as.character(mat[2, 2]), "\\cdot", as.character(mat[3, 1]), "-",
    as.character(mat[1, 1]), "\\cdot", as.character(mat[2, 3]), "\\cdot", as.character(mat[3, 2]), "-",
    as.character(mat[1, 2]), "\\cdot", as.character(mat[2, 1]), "\\cdot", as.character(mat[3, 3]),
    sep = ""
    )
  }
  det_result <- det(mat)
  ending <- "$$"
  string <- str_c(beginning, det_latex, "=", computation, "=", det_result, ending, sep = "")
  string
}

compute_null_space <- function(A) {
  m <- dim(A)[1]; n <- dim(A)[2]
  ## QR factorization and rank detection
  QR <- base::qr.default(A)
  r <- QR$rank
  ## cases 2 to 4
  if ((r < min(m, n)) || (m < n)) {
    R <- QR$qr[1:r, , drop = FALSE]
    P <- QR$pivot
    F <- R[, (r + 1):n, drop = FALSE]
    I <- base::diag(1, n - r)
    B <- -1.0 * base::backsolve(R, F, r)
    Y <- base::rbind(B, I)
    X <- Y[base::order(P), , drop = FALSE]
    return(X)
  }
  ## case 1
  return(base::matrix(0, n, 1))
}


