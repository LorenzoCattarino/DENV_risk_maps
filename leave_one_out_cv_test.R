
library(rgdal)
library(rgeos)
library(ggplot2)
library(h2o)

source(file.path("R", "random_forest", "functions_for_fitting_h2o_RF_and_making_predictions.r"))


# define parameters ----------------------------------------------------------- 


geo_crs <- CRS("+proj=longlat +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")

prj_crs <- CRS("+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")

dependent_variable <- "FOI"
pseudoAbsence_value <- -0.02
no_trees <- 500
min_node_size <- 20
all_wgt <- 1
wgt_limits <- c(1, 500)


# load data ------------------------------------------------------------------- 


foi_data <- read.csv(
  file.path("output", "foi", "All_FOI_estimates_linear_env_var_area.csv"),
  stringsAsFactors = FALSE) 

predictor_rank <- read.csv(
  file.path("output", 
            "variable_selection", 
            "metropolis_hastings", 
            "exp_1", 
            "variable_rank_final_fits_exp_1.csv"),
  stringsAsFactors = FALSE)


# remove background data ------------------------------------------------------


foi_data <- foi_data[foi_data$type != "pseudoAbsence", ]


# project points  -------------------------------------------------------------


foi_points <- SpatialPointsDataFrame(coords = foi_data[, c("longitude", "latitude")], 
                                     data = foi_data,
                                     proj4string = geo_crs)

foi_points_prj <- spTransform(foi_points, prj_crs) 


# calculate distance ----------------------------------------------------------


d <- gDistance(foi_points_prj, byid = TRUE)

min.d <- apply(d, 1, function(x) order(x, decreasing = FALSE)[2])

new_foi_data <- cbind(foi_data, neighbor = foi_data[min.d,"data_id"], 
                      distance = apply(d, 1, function(x) sort(x, decreasing = FALSE)[2]))

# from m to km 
new_foi_data$distance <- new_foi_data$distance / 1000


# histogram -------------------------------------------------------------------


p <- ggplot(new_foi_data, aes(distance)) +
  geom_histogram(binwidth = 40) +
  scale_x_continuous("distance (km)")

ggsave(file.path("figures", "closest_distance_hist.png"), p)


# fit -------------------------------------------------------------------------


my_predictors <- predictor_rank$variable[1:9]

new_foi_data$new_weight <- all_wgt

n <- nrow(new_foi_data)
  
new_foi_data$p_i <- 0

h2o.init()

for (i in seq_len(n)){
  
  message(sprintf("iteration = %s", i))
  
  point_out <- new_foi_data[i, ]
  
  training_dataset <- new_foi_data[-i, c(dependent_variable, my_predictors, "new_weight")]
  
  RF_obj <- fit_h2o_RF(dependent_variable = dependent_variable, 
                       predictors = my_predictors, 
                       training_dataset = training_dataset, 
                       no_trees = no_trees, 
                       min_node_size = min_node_size,
                       my_weights = "new_weight")
  
  validating_dataset <- point_out[, c(my_predictors, "new_weight")]
  
  p_i <- make_h2o_predictions(
    mod_obj = RF_obj, 
    dataset = validating_dataset, 
    sel_preds = my_predictors)
  
  new_foi_data[i, "p_i"] <- p_i
  
}

h2o.shutdown(prompt = FALSE)

write.csv(new_foi_data, 
          file.path("output", "foi", "new_foi_data.csv"), 
          row.names = FALSE)

probs <- seq(0, 1, 0.1)
  
quant_classes <- cut(new_foi_data$distance, 
                     quantile(new_foi_data$distance, probs = probs), 
                     include.lowest=TRUE, 
                     labels = FALSE)

new_foi_data$k <- quant_classes

output <- ddply(new_foi_data, .(k), function(x) cor(x$FOI, x$p_i))

p2 <- ggplot(output, aes(k, V1)) +
  geom_line() +
  scale_x_continuous("distance class") +
  scale_y_continuous("correlation coefficient")

ggsave(file.path("figures", "correlation_vs_distance_class.png"), p2)
