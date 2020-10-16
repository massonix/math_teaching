library(ggplot2)
u <- c(2, 2)
v <- c(4, 1)
c1 <- -2
c2 <- 1

scaled_u <- c1 * u
scaled_v <- c2 * v
lin_comb <- scaled_u + scaled_v

max_x <- max(abs(c(scaled_u[1], scaled_v[1], lin_comb[1])))
max_y <- max(abs(c(scaled_u[2], scaled_v[2], lin_comb[2])))
max_all <- max(abs(c(max_x, max_y)))

ggplot(df) +
  geom_vline(xintercept = 0, color = "darkgrey") +
  geom_hline(yintercept = 0, color = "darkgrey") +
  geom_segment(
    aes(x = 0, y = 0, xend = scaled_u[1], yend = scaled_u[2]),
    arrow = arrow(length = unit(0.03, "npc")),
    color = "red"
  ) +
  geom_segment(
    aes(x = 0, y = 0, xend = scaled_v[1], yend = scaled_v[2]),
    arrow = arrow(length = unit(0.03, "npc")),
    color = "blue"
  ) +
  geom_segment(
    aes(x = 0, y = 0, xend = lin_comb[1], yend = lin_comb[2]),
    arrow = arrow(length = unit(0.03, "npc")),
    color = "black"
  ) +
  scale_x_continuous("x", limits = c(-1 * max_all, max_all)) +
  scale_y_continuous("y", limits = c(-1 * max_all, max_all)) +
  theme_bw()




