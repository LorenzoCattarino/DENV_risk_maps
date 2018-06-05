
library(geosphere) # for distm()

source(file.path("R", "random_forest", "fit_h2o_RF_and_make_predictions.r"))


# load data ------------------------------------------------------------------- 


foi_data <- read.csv(file.path("output", 
                               "foi", 
                               "All_FOI_estimates_linear_env_var_area.csv"),
                     stringsAsFactors = FALSE) 

predictor_rank <- read.csv(file.path("output", 
                                     "variable_selection", 
                                     "metropolis_hastings", 
                                     "exp_1", 
                                     "variable_rank_final_fits_exp_1.csv"),
                           stringsAsFactors = FALSE)


# process ---------------------------------------------------------------------


foi_points <- foi_data[, c("longitude","latitude")]

d <- distm(foi_points, fun = distGeo)

min.d <- apply(d, 1, function(x) order(x, decreasing = FALSE)[2])

new_foi_data <- cbind(foi_data, neighbor = foi_data[min.d,"data_id"], 
                      distance = apply(d, 1, function(x) sort(x, decreasing = FALSE)[2]))

# from m to km 
new_foi_data$distance <- new_foi_data$distance / 1000

write.csv(new_foi_data, 
          file.path("output", "foi", "All_FOI_estimates_linear_env_var_area_dis.csv"), 
          row.names = FALSE)
