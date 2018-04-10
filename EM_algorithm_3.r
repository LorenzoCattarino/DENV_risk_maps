# Fits RF to all original foi data (using fixed RF parameters) 

options(didehpc.cluster = "fi--didemrchnb")

my_resources <- c(
  file.path("R", "utility_functions.r"),
  file.path("R", "random_forest", "fit_h2o_RF_and_make_predictions.r"),
  file.path("R", "prepare_datasets", "set_pseudo_abs_weights.R"))

my_pkgs <- "h2o"

context::context_log_start()
ctx <- context::context_save(path = "context",
                             sources = my_resources,
                             packages = my_pkgs)


# define parameters ----------------------------------------------------------- 


var_to_fit <- "FOI"

pseudoAbsence_value <- -0.02

no_trees <- 500

min_node_size <- 20

all_wgt <- 1

wgt_limits <- c(1, 500)

number_of_predictors <- 9

out_path <- file.path("output", 
                      "EM_algorithm", 
                      "best_fit_models", 
                      paste0("model_objects_", var_to_fit, "_fit"))

out_name <- "all_data.rds"   


# start up -------------------------------------------------------------------- 


context::context_load(ctx)


# load data ------------------------------------------------------------------- 


# load FOI dataset
foi_data <- read.csv(file.path("output", 
                               "foi", 
                               "All_FOI_estimates_linear_env_var_area.csv"),
                     stringsAsFactors = FALSE)

# predicting variable rank
predictor_rank <- read.csv(file.path("output", 
                                     "variable_selection", 
                                     "metropolis_hastings", 
                                     "exp_1", 
                                     "variable_rank_final_fits_exp_1.csv"),
                           stringsAsFactors = FALSE)


# pre processing -------------------------------------------------------------- 


my_predictors <- predictor_rank$name[1:number_of_predictors]

# set pseudo absence value
foi_data[foi_data$type == "pseudoAbsence", var_to_fit] <- pseudoAbsence_value

# assign weights
foi_data$new_weight <- all_wgt
pAbs_wgt <- get_area_scaled_wgts(foi_data, wgt_limits)
foi_data[foi_data$type == "pseudoAbsence", "new_weight"] <- pAbs_wgt

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


h2o.init()

RF_obj <- fit_h2o_RF(dependent_variable = var_to_fit, 
                     predictors = my_predictors, 
                     training_dataset = training_dataset, 
                     no_trees = no_trees, 
                     min_node_size = min_node_size,
                     my_weights = "new_weight",
                     model_nm = out_name)

h2o.saveModel(RF_obj, out_path, force = TRUE)

h2o.shutdown(prompt = FALSE)
