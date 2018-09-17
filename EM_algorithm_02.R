# Fits RF to all original foi data (using fixed RF parameters) 

options(didehpc.cluster = "fi--didemrchnb")

my_resources <- c(
  file.path("R", "random_forest", "fit_ranger_RF_and_make_predictions.R"),
  file.path("R", "prepare_datasets", "set_pseudo_abs_weights.R"),
  file.path("R", "utility_functions.R"))

my_pkgs <- "ranger"

context::context_log_start()
ctx <- context::context_save(path = "context",
                             sources = my_resources,
                             packages = my_pkgs)


# define parameters ----------------------------------------------------------- 


parameters <- list(
  dependent_variable = "R0_3",
  shape_1 = 0,
  shape_2 = 5,
  shape_3 = 1.6e6,
  pseudoAbs_value = 0.5,
  foi_offset = 0.03,
  no_trees = 500,
  min_node_size = 20,
  all_wgt = 1,
  wgt_limits = c(1, 500),
  no_predictors = 26) 

out_name <- "all_data.rds"  

foi_dts_nm <- "All_FOI_estimates_and_predictors.csv"

extra_predictors <- NULL


# define variables ------------------------------------------------------------


var_to_fit <- parameters$dependent_variable

foi_offset <- parameters$foi_offset
  
out_path <- file.path("output", 
                      "EM_algorithm", 
                      "best_fit_models", 
                      paste0("model_objects_", var_to_fit, "_fit"))


# start up -------------------------------------------------------------------- 


context::context_load(ctx)


# load data ------------------------------------------------------------------- 


foi_data <- read.csv(file.path("output", "foi", foi_dts_nm),
                     stringsAsFactors = FALSE)

predictor_rank <- read.csv(file.path("output", 
                                     "variable_selection",
                                     "stepwise",
                                     "predictor_rank.csv"), 
                           stringsAsFactors = FALSE)


# pre processing -------------------------------------------------------------- 


# set pseudo absence value
foi_data[foi_data$type == "pseudoAbsence", var_to_fit] <- parameters$pseudoAbs_value

# assign weights
foi_data$new_weight <- parameters$all_wgt
pAbs_wgt <- get_sat_area_wgts(foi_data, parameters)
foi_data[foi_data$type == "pseudoAbsence", "new_weight"] <- pAbs_wgt

my_predictors <- predictor_rank$name[1:parameters$no_predictors]
my_predictors <- c(my_predictors, extra_predictors)

if(var_to_fit == "FOI"){
  
  foi_data[, var_to_fit] <- foi_data[, var_to_fit] + foi_offset

}

# get training dataset (full dataset - no bootstrap)
training_dataset <- foi_data[, c(var_to_fit, my_predictors, "new_weight")]


####
# plot

# library(ggplot2)
# 
# sub_foi <- foi_data[foi_data$type == "pseudoAbsence",]
# 
# png(file.path("figures", "wgt_area_relationship.png"),
#     width = 10,
#     height = 6,
#     units = "in",
#     pointsize = 12,
#     res = 200)
# 
# p <- ggplot(sub_foi, aes(Shape_Area, new_weight)) +
#   geom_point() +
#   geom_text(aes(label = ISO), nudge_y = -0.5, size = 1.8) +
#   scale_y_continuous(name = "Observation weight",
#                      limits = c(0, max(wgt_limits)),
#                      expand = c(0.1,0.1),
#                      breaks = pretty(sub_foi$new_weight, 4),
#                      labels = pretty(sub_foi$new_weight, 4)) +
#   scale_x_continuous(name = expression("Pseudo absence admin unit area " ~ (10^{3} ~ "km"^{2})),
#                      breaks = pretty(sub_foi$Shape_Area, 4),
#                      labels = format(pretty(sub_foi$Shape_Area, 4) / 1000, scientific = FALSE))
# 
# print(p)
# dev.off()


# run job --------------------------------------------------------------------- 


RF_obj <- fit_ranger_RF(dependent_variable = parameters$dependent_variable, 
                        predictors = my_predictors, 
                        training_dataset = training_dataset, 
                        no_trees = parameters$no_trees, 
                        min_node_size = parameters$min_node_size,
                        my_weights = "new_weight")

write_out_rds(RF_obj, out_path, out_name)
