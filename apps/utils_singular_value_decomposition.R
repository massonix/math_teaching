library(ggforce)
library(dplyr)
b1 <- c(1, 0)
b2 <- c(0, 1)
df <- data.frame(x = c(b1[1], b2[1]), y = c(b1[2], b2[2]))


ggplot() +
  geom_segment(aes(x = 0, y = 0, xend = 1, yend = 0), color = "red", arrow = arrow(length = unit(0.3, "inches"))) +
  geom_segment(aes(x = 0, y = 0, xend = 0, yend = 1), color = "blue", arrow = arrow(length = unit(0.3, "inches"))) +
  geom_line() +
  geom_ellipse(aes(x0 = 0, y0 = 0, a = 1, b = 1, angle = 0)) +
  coord_fixed() +
  labs(x = "x", y = "y") +
  theme_bw()
