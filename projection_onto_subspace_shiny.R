# Load packages
library(plotly)
library(matlib)
library(reshape2)

# Source utils
setwd("~/Google Drive/single_cell/PhD/trainings/math_teaching/math_teaching/")
source("utils_projections_onto_subspaces.R")

# Define inputs
A <- matrix(c(1, 0, 0, 0, 1, 0), nrow = 3, ncol = 2)
b <- c(2, 2, 2)

# Find projection (p) of b on column space of A (C(A))
p <- A %*% inv(t(A) %*% A) %*% t(A) %*% as.matrix(b)

# Compute C(A)
limits <- c((-1 * max(c(b, p)) * 1.25), (1 * max(c(b, p)) * 1.1))
step <- abs(limits[1] - limits[2]) / 10
iterable <- seq(from = limits[1], to = limits[2], by = step)
col_space_df <- data.frame(x = c(), y = c(), z = c())
norm_vector_1 <- A[, 1] / norm_vec(A[, 1])
norm_vector_1 <- A[, 1] / norm_vec(A[, 1])
for (i in iterable) {
  for(j in iterable) {
    vect <- i * A[, 1] + j * A[, 2]
    row <- data.frame(x = vect[1], y = vect[2], z = vect[3])
    col_space_df <- rbind(col_space_df, row)
  }
}
# col_space_df <- col_space_df[col_space_df$z > limits[1] & col_space_df$z < limits[2], ]
col_space_surf <- acast(col_space_df, y ~ x, value.var = "z")

# Plot C(A)
plot_ly() %>%
  add_trace(x = c(0, b[1]), y = c(0, b[2]), z = c(0, b[3]), type = "scatter3d", mode = "marker+line", name = "b",
            marker = list(size = 10, symbol = "diamond"), line = list(size = 2000, width = 5)) %>%
  add_trace(x = c(0, p[1]), y = c(0, p[2]), z = c(0, p[3]), type = "scatter3d", mode = "line", name = "p",
            marker = list(size = 10, symbol = "diamond"), line = list(size = 2000, width = 5)) %>%
  add_trace(x = c(b[1], p[1]), y = c(b[2], p[2]), z = c(b[3], p[3]), type = "scatter3d", mode = "line", name = "e",
            marker = list(size = 0.1), line = list(size = 2000, width = 6, height = 6, dash = "dot")) %>% 
  add_trace(z = col_space_surf, x = iterable, y = iterable, type = "surface", colorscale = list(c(0, 1), c("black", "gray")),
            showscale = FALSE)




