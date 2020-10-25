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
  cols <- c("blue", "green", "red", "yellow")
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

plot_point_2d <- function(x) {
  p <- plot_ly(x = x[1, 1], y = x[2, 1]) %>% 
    add_markers()
  p
}

plot_line_2d <- function(x) {
  scalars <- seq(-1, 1, by = 0.05)
  points <- map(scalars, function(k) as.vector(k * x))
  points <- t(as.data.frame(points))
  rownames(points) <- NULL
  colnames(points) <- c("x", "y")
  p <- as.data.frame(points) %>% 
    plot_ly(x = ~x, y = ~y) %>% 
    add_lines()
  p
}

plot_point_3d <- function(x) {
  df <- data.frame(x = x[1, 1], y = x[2, 1], z = x[3, 1])
  p <- df %>% 
    plot_ly(x = ~x, y = ~y, z = ~z) %>% 
    add_markers()
  p
}

plot_line_3d <- function(x) {
  scalars <- seq(-1, 1, by = 0.01)
  points <- map(scalars, function(k) k * x[, 1])
  points <- t(as.data.frame(points))
  rownames(points) <- NULL
  colnames(points) <- c("x", "y", "z")
  points <- rbind(points, data.frame(x = 0, y = 0, z = 0))
  points <- as.data.frame(points)
  p <- points %>%
    plot_ly(x = ~x, y = ~y, z = ~z) %>% 
    add_lines()
  p
}

plot_plane_3d <- function(A) {
  scalars <- seq(-1, 1, by = 0.05)
  vectors <- vector("list", length = length(scalars) ** 2)
  count <- 0
  for (i in scalars) {
    for(j in scalars) {
      count <- count + 1
      vectors[[count]] <- i * A[, 1] + j * A[, 2]
    }
  }
  points <- t(as.data.frame(vectors))
  rownames(points) <- NULL
  colnames(points) <- c("x", "y", "z")
  points <- rbind(points, data.frame(x = 0, y = 0, z = 0))
  p <- points %>% 
    plot_ly(x = ~x, y = ~y, z = ~z) %>% 
    add_markers()
  p
}

plot_null_space_2d <- function(A) {
  null_space <- compute_null_space(A)
  if (all(null_space == c(0, 0))) {
    p <- plot_point_2d(null_space)
  } else if (all(dim(null_space) == c(2, 1))) {
    p <- plot_line_2d(null_space)
  }
  p
}

plot_null_space_3d <- function(A) {
  null_space <- compute_null_space(A)
  if (all(null_space == c(0, 0, 0))) {
    p <- plot_point_3d(null_space)
  } else if (all(dim(null_space) == c(3, 1))) {
    p <- plot_line_3d(null_space)
  } else if (all(dim(null_space) == c(3, 2))) {
    p <- plot_plane_3d(null_space)
  }
  p
}

find_inverse <- function(A) {
  if (det(A) == 0) {
    "As the determinant is 0, the matrix is not invertible"
  } else {
    solve(A)
  }
}

# adjust_column <- function(A, coords) {
#   nums <- c(A[coords[[1]][1], coords[[1]][2]], A[coords[[2]][1], coords[[2]][2]])
#   if (nums[1] != nums[2]) {
#     greater_num <- max(nums)
#     smaller_num <- min(nums)
#     col_greater <- coords[[which(nums == greater_num)]][2]
#     col_smaller <- coords[[which(nums == smaller_num)]][2]
#   } else {
#     greater_num <- nums[1]
#     smaller_num <- nums[1]
#     col_greater <- coords[[1]][2]
#     col_smaller <- coords[[2]][2]
#   }
#   
#   if (greater_num == smaller_num) {
#     A[, coords[[2]][2]] <- -1 * A[, col_smaller] + A[, col_greater]
#     return(A)
#   } else if (greater_num %% smaller_num == 0) {
#     fact <- greater_num
#     fact <- ifelse(sign(nums[1]) == sign(nums[2]), -1 * fact, fact)
#     A[, coords[[2]][2]] <- fact * A[, col_smaller] + A[, col_greater]
#     return(A)
#   } else {
#     fact1 <- ifelse(sign(nums[1]) == sign(nums[2]), -1 * nums[2], nums[2])
#     fact2 <- nums[1]
#     A[, coords[[2]][2]] <- fact2 * A[, coords[[2]][1]] + fact1 * A[, coords[[2]][1]]
#     return(A)
#   }
# }

# apply_gauss_3d <- function(A) {
#   if (A[1, 1] != 0 & A[1, 2] != 0) {
#     A <- adjust_column(A, coords = list(c(1, 1), c(1, 2)))
#     A
#   } else if (A[1, 1] != 0 & A[1, 3] != 0) {
#     A <- adjust_column(A, coords = list(c(1, 1), c(1, 3)))
#     A
#   } else if (A[2, 2] != 0 & A[2, 3] != 0) {
#     A <- adjust_column(A, coords = list(c(2, 2), c(2, 3)))
#     A
#   } else {
#     A
#   }
# }

# combine_columns <- function(x, y, entry) {
#   if (x[entry] == y[entry]) {
#     x_multiple <- 1
#     y_multiple <- -1
#   } else if (x[entry] %% y[entry] == 0) {
#     x_multiple <- 1
#     y_multiple <- -1 * x[entry]
#   } else if (y[entry] %% x[entry] == 0) {
#     x_multiple <- -1 * y[entry]
#     y_multiple <- 1
#   } else {
#     x_multiple <- -1 * y[entry]
#     y_multiple <- x[entry]
#   }
#   new_column <- (x_multiple * x) + (y_multiple * y)
#   new_column
# }
apply_gauss_jordan_2d <- function(A, I) {
  # From A --> L
  if (A[1, 1] != 0 & A[1, 2] != 0) {
    mults <- find_multiples(x = A[, 1], A[, 2], entry = 1)
    A[, 2] <- combine_columns(x = A[, 1], y = A[, 2], x_m = mults["x"], y_m = mults["y"])
    I[, 2] <- combine_columns(x = I[, 1], y = I[, 2], x_m = mults["x"], y_m = mults["y"])
    assignment_message <- str_c("C2", " \U2190 ", fractions(mults["x"]), "C1",
                                " + ", fractions(mults["y"]), "C2", " \n", sep = "")
    output <- list(A = A, I = I, assignment_message = assignment_message)
    return(output)
  }
  
  # Force pivots = 1
  pivots <- c(A[1, 1], A[2, 2])
  for (i in 1:length(pivots)) {
    if (pivots[i] != 1 & pivots[i] != 0) {
      A[, i] <- A[, i] / pivots[i]
      I[, i] <- I[, i] / pivots[i]
      assignment_message <- str_c("C", i, " \U2190 ", fractions(1 / pivots[i]), "C", i, " \n", sep = "")
      output <- list(A = A, I = I, assignment_message = assignment_message)
      return(output)
    }
  }
  
  # From L --> R
  if (A[2, 1] != 0 & A[2, 2]) {
    mults <- find_multiples(x = A[, 2], A[, 1], entry = 2)
    A[, 1] <- combine_columns(x = A[, 2], y = A[, 1], x_m = mults["x"], y_m = mults["y"])
    I[, 1] <- combine_columns(x = I[, 2], y = I[, 1], x_m = mults["x"], y_m = mults["y"])
    assignment_message <- str_c("C1", " \U2190 ", fractions(mults["y"]), "C1",
                                " + ", fractions(mults["x"]), "C2", " \n", sep = "")
    output <- list(A = A, I = I, assignment_message = assignment_message)
    return(output)
  }
  
  # If A is already in L form, return it
  output <- list(A = A, I = I, assignment_message = "")
  return(output)
}


apply_gauss_jordan_3d <- function(A, I) {
  # From A --> L
  ## Find zeros first row
  for (i in 1:2) {
    if (A[1, i] == 0) {
      next
    } else {
      for (j in (i + 1):ncol(A)) {
        if (A[1, j] == 0) {
          next
        } else {
          mults <- find_multiples(x = A[, i], y = A[, j], entry = 1)
          output <- map(list(A, I), function(mat) {
            mat[, j] <- combine_columns(x = mat[, i], y = mat[, j], x_m = mults["x"], y_m = mults["y"])
            mat
          })
          assignment_message <- str_c("C", j, " \U2190 ", fractions(mults["x"]), "C", i,
                                      " + ", fractions(mults["y"]), "C", j, " \n", sep = "")
          output <- c(output, assignment_message)
          return(output)
        }
      }
    }
  }
  ## Find 0 in the element a23
  if (A[2, 3] != 0) {
    if (A[2, 2] != 0 & A[1, 2] == 0) {
      mults <- find_multiples(x = A[, 2], y = A[, 3], entry = 2)
      output <- map(list(A, I), function(mat) {
        mat[, 3] <- combine_columns(x = mat[, 2], y = mat[, 3], x_m = mults["x"], y_m = mults["y"])
        mat
      })
      assignment_message <- str_c("C3", " \U2190 ", fractions(mults["x"]), "C2",
                                  " + ", fractions(mults["y"]), "C3", " \n", sep = "")
      output <- c(output, assignment_message)
      return(output)
    } else if (A[2, 1] != 0 & A[1, 1] == 0) {
      mults <- find_multiples(x = A[, 1], y = A[, 3], entry = 2)
      output <- map(list(A, I), function(mat) {
        mat[, 3] <- combine_columns(x = mat[, 1], y = mat[, 3], x_m = mults["x"], y_m = mults["y"])
        mat
      })
      assignment_message <- str_c("C3", " \U2190 ", fractions(mults["x"]), "C1", " + ",
                                  fractions(mults["y"]), "C3", " \n", sep = "")
      output <- c(output, assignment_message)
      return(output)
    }
  }
  
  ## Simplify to force pivots = 1
  ## Potentially at this point: PERMUTE TO GET INTO L form
  pivots <- c(A[1, 1], A[2, 2], A[3, 3])
  for (i in 1:3) {
    if (pivots[i] != 0 & pivots[i] != 1) {
      A[, i] <- A[, i] / pivots[i]
      I[, i] <- I[, i] / pivots[i]
      output <- list(A, I)
      assignment_message <- str_c("C", i, " \U2190 ", fractions(1/pivots[i]), "C", i, " \n", sep = "")
      output <- c(output, assignment_message)
      return(output)
    }
  }
  
  # From L --> R
  for (i in 3:2) {
    if (pivots[i] != 0) {
      for (j in (i-1):1) {
        if (A[i, j] != 0) {
          mults <- find_multiples(x = A[, j], y = A[, i], entry = i)
          output <- map(list(A, I), function(mat) {
            mat[, j] <- combine_columns(x = mat[, j], y = mat[, i], x_m = mults["x"], y_m = mults["y"])
            mat
          })
          assignment_message <- str_c("C", j, " \U2190 ", fractions(mults["x"]), "C", j, " + ",
                                      fractions(mults["y"]), "C", i, " \n", sep = "")
          output <- c(output, assignment_message)
          return(output)
        }
      }
    }
  }
  
  # If A is already reduced, return it
  output <- list(A, I, "")
  return(output)
}


find_multiples <- function(x, y, entry) {
  if (x[entry] == y[entry]) {
    x_multiple <- 1
    y_multiple <- -1
  } else if (x[entry] %% y[entry] == 0) {
    x_multiple <- 1
    y_multiple <- -1 * x[entry]
  } else if (y[entry] %% x[entry] == 0) {
    x_multiple <- -1 * y[entry]
    y_multiple <- 1
  } else {
    x_multiple <- -1 * y[entry]
    y_multiple <- x[entry]
  }
  c(x = x_multiple, y = y_multiple)
}

combine_columns <- function(x, y, x_m, y_m) {
  new_column <- (x_m * x) + (y_m * y)
  new_column
}
