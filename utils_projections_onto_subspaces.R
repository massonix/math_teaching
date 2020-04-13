###############################################################################
############## Functions Shiny app on Projections onto Subspaces ##############
###############################################################################

norm_vec <- function(x) sqrt(sum(x^2))


calculate_col_space <- function(A, b, p) {
  limits <- c(
    (-1 * max(c(norm_vec(b), norm_vec(p))) * 1.25),
    (1 * max(c(norm_vec(b), norm_vec(p))) * 1.25)
  )
  step <- abs(limits[1] - limits[2]) / 10
  iterable <- seq(from = limits[1], to = limits[2], by = step)
  col_space_df <- data.frame(x = c(), y = c(), z = c())
  norm_vector_1 <- A[, 1] / norm_vec(A[, 1])
  norm_vector_2 <- A[, 2] / norm_vec(A[, 2])
  for (i in iterable) {
    for(j in iterable) {
      vect <- (i * norm_vector_1) + (j * norm_vector_2)
      row <- data.frame(x = vect[1], y = vect[2], z = vect[3])
      col_space_df <- rbind(col_space_df, row)
    }
  }
  col_space_df
}

plot_projection <- function(b, p, col_space_df) {
  plot_ly() %>%
  add_trace(x = c(0, b[1]), y = c(0, b[2]), z = c(0, b[3]), type = "scatter3d", mode = "marker+line", name = "b",
            marker = list(size = 10, symbol = "diamond"), line = list(size = 2000, width = 5)) %>%
  add_trace(x = c(0, p[1]), y = c(0, p[2]), z = c(0, p[3]), type = "scatter3d", mode = "line", name = "p",
            marker = list(size = 10, symbol = "diamond"), line = list(size = 2000, width = 5)) %>%
  add_trace(x = c(b[1], p[1]), y = c(b[2], p[2]), z = c(b[3], p[3]), type = "scatter3d", mode = "line", name = "e",
            marker = list(size = 0.1), line = list(size = 2000, width = 6, height = 6, dash = "dot")) %>%
  add_mesh(x = ~x, y = ~y, z = ~z, data = col_space_df, opacity = 0.1)
}
