library(ggplot2)
library(dplyr)

x <- runif(10000, 0, 20)

data_df <- data.frame(time = x, sin_x = sin(x), cos_x = cos(x))

data_df_long <- melt(
  data_df, 
  id.vars = setdiff(names(data_df), c("sin_x", "cos_x")), 
  variable.name = "fun",
  value.name = "function_value")

png(file.path("figures", "sin_cos_figure.png"), 
    width = 16.5, 
    height = 8, 
    units = "cm", 
    pointsize = 12,
    res = 300)

p <- ggplot() +
  geom_line(data = data_df_long, 
            aes(time, function_value, colour = fun), 
            size = 1.4)

print(p) 

dev.off()
