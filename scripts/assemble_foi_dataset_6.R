# Computes the min distance between each data point and any other data points

library(geosphere) # for distm()


# load data ------------------------------------------------------------------- 


foi_data <- read.csv(file.path("output", 
                               "foi", 
                               "All_FOI_estimates_and_predictors.csv"),
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
          file.path("output", "foi", "All_FOI_estimates_and_predictors_dis.csv"), 
          row.names = FALSE)
