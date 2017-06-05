library(ggplot2)
library(dplyr)

x = runif(10000, 0, 20)

data_df <- data.frame(time = x, sin_x = sin(x), cos_x = cos(x))

data_df_long <- melt(
  data_df, 
  id.vars = setdiff(names(data_df), c("sin_x", "cos_x")), 
  variable.name = "fun",
  value.name = "function_value")

ggplot(data_df_long, aes(time, function_value, colour = fun)) +
  geom_line(size = 1.4)

ggsave(file.path("figures", "sin_cos_figure.png"))
