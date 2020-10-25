library(ggforce)
library(dplyr)


norm_vec <- function(x) sqrt(sum(x^2))


plot_circle_svd <- function(i, j, ang) {
  norm_i <- norm_vec(i)
  norm_j <- norm_vec(j)
  p <- ggplot() +
    geom_segment(
      aes(x = 0, y = 0, xend = i[1], yend = i[2]),
      color = "red",
      arrow = arrow(length = unit(0.3, "inches"))
    ) +
    geom_segment(
      aes(x = 0, y = 0, xend = j[1], yend = j[2]),
      color = "blue",
      arrow = arrow(length = unit(0.3, "inches"))
    ) +
    geom_line() +
    geom_ellipse(aes(x0 = 0, y0 = 0, a = norm_i, b = norm_j, angle = ang)) +
    coord_fixed() +
    labs(x = "x", y = "y") +
    theme_bw()
  p
}

# Initialize
mat <- matrix(c(1,2,2,4), 2, 2, byrow = FALSE)
svd_mat <- svd(mat)
i <- svd_mat$v[, 1]
j <- svd_mat$v[, 2]
initial_circle <- plot_circle_svd(i, j, ang = 0)
initial_circle


# Multiply by Vt
vt_i <- drop(t(svd_mat$v) %*% i)
vt_j <- drop(t(svd_mat$v) %*% j)
vt_circle <- plot_circle_svd(vt_i, vt_j, ang = 0)
vt_circle


# Multiply by D
D_mat <- matrix(c(svd_mat$d[1], 0, 0, svd_mat$d[2]), 2, 2, byrow = FALSE)
d_vt_i <- drop(D_mat %*% vt_i)
d_vt_j <- drop(D_mat %*% vt_j)
d_vt_circle <- plot_circle_svd(d_vt_i, d_vt_j, ang = 0)
d_vt_circle


# Multiply by U
u_d_vt_i <- drop(svd_mat$u %*% d_vt_i)
u_d_vt_j <- drop(svd_mat$u %*% d_vt_j)
ang1 <- drop(matlib::angle(d_vt_i, u_d_vt_i, degree = FALSE))
u_d_vt_circle <- plot_circle_svd(u_d_vt_i, u_d_vt_j, ang = -1*ang1)
u_d_vt_circle

