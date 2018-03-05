# Plot the train and set points for different grid sizes 

library(ggplot2)

foi_data <- read.csv(
  file.path("output", "foi", "All_FOI_estimates_linear_env_var_area.csv"),
  stringsAsFactors = FALSE) 

boot_samples <- readRDS(
  file.path("output",
            "disagg_data_20km",
            "bootstrap_samples.rds"))

all_points <- foi_data$data_id
  
in_points <- boot_samples[[139]]$data_id

unique_in_points <- unique(in_points)

out_points <- all_points[!all_points %in% unique_in_points]

in_df <- foi_data[unique_in_points, ]
out_df <- foi_data[out_points, ]

in_df$set <- "train"
out_df$set <- "valid"

comb <- rbind(in_df, out_df)
  
p <- ggplot() +
  geom_point(data = comb, aes(x = longitude, y = latitude, colour = set), size = 0.5) +
  coord_equal() + 
  theme_void()

ggsave(file.path("figures", 
                 "sensitivity_analysis", 
                 "grid_size", 
                 "train_and_set_points",
                 "grid_10km.png"), 
       p, 
       width = 15, 
       height = 8, 
       units = "cm")
