library(plotly)
library(dplyr)

input_matrix <- matrix(
  c(c(0, 0, 1, 1, 0, 0, 1, 1), c(0, 1, 1, 0, 0, 1, 1, 0), c(0, 0, 0, 0, 1, 1, 1, 1)),
  nrow = 3, 
  ncol = 8,
  byrow = TRUE
)
transformation_matrix <- matrix(c(2, 0, 0, 0, 2, 0, 0, 0, 2), 3, 3, byrow = TRUE)
output_matrix <- transformation_matrix %*% input_matrix
input_df <- as.data.frame(t(input_matrix))
colnames(input_df) <- c("x", "y", "z")
output_df <- as.data.frame(t(output_matrix))
colnames(output_df) <- c("x", "y", "z")
df <- bind_rows(list(input = input_df, output = output_df), .id = "status")
axx <- list(nticks = 6, range = c(0, 4))
axy <- list(nticks = 6, range = c(0, 4))
axz <- list(nticks = 6, range = c(0, 4))
df %>% 
  filter(status == "output") %>% 
  plot_ly(x = ~x, y = ~y, z = ~z) %>% 
  add_mesh(
    i = c(7, 0, 0, 0, 4, 4, 6, 6, 4, 0, 3, 2),
    j = c(3, 4, 1, 2, 5, 6, 5, 2, 0, 1, 6, 3),
    k = c(0, 7, 2, 3, 6, 7, 1, 1, 5, 5, 7, 6),
    intensity = seq(0, 1, length = 8),
    color = seq(0, 1, length = 8),
    colors = colorRamp(rainbow(8))
  ) %>% 
  layout(scene = list(xaxis = axx, yaxis = axy, zaxis = axz))


######
#####2D#######
input_matrix <- matrix(
  c(c(0, 1, 1, 0), c(0, 0, 1, 1)),
  nrow = 2, 
  ncol = 4,
  byrow = TRUE
)
transformation_matrix <- matrix(c(2, 0, 0, 2), 2, 2, byrow = TRUE)
output_matrix <- transformation_matrix %*% input_matrix
input_df <- as.data.frame(t(input_matrix))
colnames(input_df) <- c("x", "y")
output_df <- as.data.frame(t(output_matrix))
colnames(output_df) <- c("x", "y")
df <- bind_rows(list(input = input_df, output = output_df), .id = "status")

p <- df %>% 
  filter(status == "input") %>% 
  plot_ly(x = ~x, y = ~y) %>%
  add_trace(
    type = 'scatter',
    fill = 'toself',
    fillcolor = '#ab63fa',
    hoveron = 'points+fills',
    marker = list(
      color = '#ab63fa'
    ),
    line = list(
      color = '#ab63fa'
    ),
    text = "Points + Fills",
    hoverinfo = 'text'
  ) 


# Missing: create subplot (input and output next to each other with title)

# Order
# 1. Learn to create subplots for input/output that share same scale. Option: create two plotOutputs in the ShinyApp()
# 2. Create 2D transformations with filled area plots. "Shapes" (rectangle) and adding markers to the vertices.
# 3. Create a script with functions that will be sourced in the shinny app. First functions: plot_3D_transformation/plot_2D_transformation.
# X. Create line if transformation has all column vectors linearly dependent
    