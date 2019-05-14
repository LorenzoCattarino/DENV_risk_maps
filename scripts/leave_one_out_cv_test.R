
library(rgdal)
library(ggplot2)
library(h2o)

source(file.path("R", "random_forest", "fit_h2o_RF_and_make_predictions.r"))


# define parameters ----------------------------------------------------------- 


dependent_variable <- "FOI"
no_trees <- 500
min_node_size <- 20
all_wgt <- 1


# load data ------------------------------------------------------------------- 


foi_data <- read.csv(file.path("output", 
                               "foi", 
                               "All_FOI_estimates_linear_env_var_area_dis.csv"),
                     stringsAsFactors = FALSE) 

predictor_rank <- read.csv(file.path("output", 
                                     "variable_selection", 
                                     "metropolis_hastings", 
                                     "exp_1", 
                                     "variable_rank_final_fits_exp_1.csv"),
                           stringsAsFactors = FALSE)


# remove background data ------------------------------------------------------


foi_data <- foi_data[foi_data$type != "pseudoAbsence", ]


# histogram -------------------------------------------------------------------


pretty_x_vals <- pretty(foi_data$distance, 15)

p <- ggplot(foi_data, aes(distance)) +
  geom_histogram(binwidth = 40) +
  scale_x_continuous("distance (km)",
                     breaks = pretty_x_vals,
                     labels = pretty_x_vals,
                     limits = c(min(pretty_x_vals), max(pretty_x_vals)))

ggsave(file.path("figures", "closest_distance_hist.png"), 
       p, 
       width = 15, 
       height = 8, 
       units = "cm")


# fit -------------------------------------------------------------------------


my_predictors <- predictor_rank$variable[1:9]

foi_data$new_weight <- all_wgt

n <- nrow(foi_data)
  
foi_data$p_i <- 0

h2o.init()

for (i in seq_len(n)){
  
  message(sprintf("iteration = %s", i))
  
  point_out <- foi_data[i, ]
  
  training_dataset <- foi_data[-i, c(dependent_variable, my_predictors, "new_weight")]
  
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
  
  foi_data[i, "p_i"] <- p_i
  
}

h2o.shutdown(prompt = FALSE)

probs <- seq(0, 1, 0.1)
  
quant_classes <- cut(foi_data$distance, 
                     quantile(foi_data$distance, probs = probs), 
                     include.lowest=TRUE, 
                     labels = FALSE)

foi_data$k <- quant_classes

output <- ddply(foi_data, .(k), function(x) cor(x$FOI, x$p_i))

p2 <- ggplot(output, aes(k, V1)) +
  geom_line() +
  scale_x_continuous("distance class") +
  scale_y_continuous("correlation coefficient")

ggsave(file.path("figures", "correlation_vs_distance_class.png"), 
       p2, 
       width = 15, 
       height = 8, 
       units = "cm")
