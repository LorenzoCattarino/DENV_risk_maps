
# calculate minimum distance between each square and the data 
# and attach it to the square dataset

library(geosphere) # for distm()
library(ggplot2)


# load data ------------------------------------------------------------------- 


foi_data1 <- read.csv(file.path("output", 
                               "foi", 
                               "All_FOI_estimates_and_predictors.csv"),
                     stringsAsFactors = FALSE) 

all_sqr_covariates <- readRDS(file.path("output", 
                                        "env_variables", 
                                        "all_squares_env_var_0_1667_deg.rds"))


# pre processing --------------------------------------------------------------


foi_data <- as.matrix(foi_data1[, c("longitude","latitude")])

all_sqr_covariates <- as.matrix(all_sqr_covariates)

foi_points <- foi_data[, c("longitude","latitude")]
squares <- all_sqr_covariates[, c("longitude","latitude")]


# calculate matrix of distances -----------------------------------------------


d <- distm(squares, foi_points, fun = distGeo)


# attach to dataset -----------------------------------------------------------


min.d <- apply(d, 1, function(x) order(x, decreasing = FALSE)[1])

all_sqr_covariates <- cbind(all_sqr_covariates,
                            neighbor = foi_data1[min.d, "data_id"],
                            distance = apply(d, 1, function(x) sort(x, decreasing = FALSE)[1]))

all_sqr_covariates[, "distance"] <- all_sqr_covariates[, "distance"] / 1000

all_sqr_covariates_df <- as.data.frame(all_sqr_covariates)

saveRDS(all_sqr_covariates_df, 
        file.path("output", 
                  "env_variables", 
                  "all_squares_env_var_0_1667_deg.rds"))


# histogram -------------------------------------------------------------------


pretty_x_vals <- pretty(all_sqr_covariates_df[, "distance"], 15)

p <- ggplot(all_sqr_covariates_df, aes(distance)) +
  geom_histogram(binwidth = 40) +
  scale_x_continuous("distance (km)",
                     breaks = pretty_x_vals,
                     labels = pretty_x_vals,
                     limits = c(min(pretty_x_vals), max(pretty_x_vals)))

ggsave(file.path("figures", "min_distance_sqr_to_data.png"), 
       p, 
       width = 15, 
       height = 8, 
       units = "cm")
