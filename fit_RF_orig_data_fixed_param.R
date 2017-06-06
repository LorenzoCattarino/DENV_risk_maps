options(didehpc.cluster = "fi--didemrchnb")

CLUSTER <- TRUE

my_resources <- c(
  file.path("R", "utility_functions.r"),
  file.path("R", "random_forest", "fit_random_forest_model.r"),
  file.path("R", "random_forest", "make_RF_predictions.r"),
  file.path("R", "random_forest", "get_1_0_point_position.r"),
  file.path("R", "random_forest", "calculate_sum_squared_errors.r"),
  file.path("R", "random_forest", "spatial_sampK_cv_rng3.r"),
  file.path("R", "random_forest", "grid_up_foi_dataset.r"),
  file.path("R", "random_forest", "bootstrap_foi_dataset.r"),
  file.path("R", "random_forest", "wrapper_to_spatial_sampK_cv_rng3.r"))

my_pkgs <- "ranger"

context::context_log_start()
ctx <- context::context_save(path = "context",
                             sources = my_resources,
                             packages = my_pkgs)


# ---------------------------------------- define parameters


y_var <- "FOI"

no_trees <- 500

min_node_size <- 20

pseudoAbs_value <- -0.02

all_wgt <- 1

pAbs_wgt <- 0.25

out_path <- file.path("output", "model_objects")

out_name <- "list_of_boot_models.rds"   
          
gr_size <- 5

no_fits <- 200


# ---------------------------------------- Are you using the cluster?


if (CLUSTER) {
  
  #config <- didewin::didewin_config(template = "24Core")
  obj <- didehpc::queue_didehpc(ctx)

} else {
  
  context::context_load(ctx)
  context::start_parallel_cluster(8, ctx)

}


# ---------------------------------------- Load data


# load FOI dataset
foi_data <- read.csv(
  file.path("output", "foi", "All_FOI_estimates_linear_env_var.csv"),
  stringsAsFactors = FALSE)

# predicting variable rank
predictor_rank <- read.csv(
  file.path("output", 
            "variable_selection", 
            "metropolis_hastings", 
            "exp_1", 
            "variable_rank_final_fits_exp_1.csv"),
  stringsAsFactors = FALSE)


# ---------------------------------------- get the vector of best predictors


my_predictors <- predictor_rank$variable[1:9]


# ---------------------------------------- pre process the original foi dataset


# remove outliers 
foi_data <- subset(foi_data, ISO != "PYF" & ISO != "HTI")

# set pseudo absence value
foi_data[foi_data$type == "pseudoAbsence", y_var] <- pseudoAbs_value

# assign weights
foi_data$new_weight <- all_wgt
foi_data[foi_data$type == "pseudoAbsence", "new_weight"] <- pAbs_wgt

# add an ID for each data point (used by get_training_point_positions() and get_validating_point_positions())
foi_data <- cbind(id = seq_len(nrow(foi_data)), foi_data)


# ---------------------------------------- create objects needed for run


# no_data <- nrow(foi_data)
# 
# # get the position (1/0) of the points in the validating dataset
# valid_point_pos <- get_validating_point_positions(no_data, training_dataset)
# 
# # get training dataset (full dataset - no bootstrap)
# training_dataset <- foi_data[, c(y_var, my_predictors)]

y_data <- foi_data[, y_var]

x_data <- foi_data[, my_predictors]


# ------------------------------------- Run RF fits


# run one job

# RF_fit <- obj$enqueue(
#   wrapper_to_core_fun(seq_len(no_fits)[1],
#                       model_dataset = foi_data, 
#                       grid_size = gr_size, 
#                       predictors = my_predictors,
#                       dependent_variable = y_var, 
#                       no_trees = no_trees, 
#                       min_node_size = min_node_size, 
#                       x_data = x_data, 
#                       y_data = y_data))

if (CLUSTER) {
  
  RF_fit <- queuer::qlapply(
    seq_len(no_fits), 
    wrapper_to_core_fun, 
    obj,
    model_dataset = foi_data, 
    grid_size = gr_size, 
    predictors = my_predictors,
    dependent_variable = y_var, 
    no_trees = no_trees, 
    min_node_size = min_node_size, 
    x_data = x_data, 
    y_data = y_data)

} else {
  
  RF_fit <- lapply(seq_len(no_fits), 
                   wrapper_to_core_fun, 
                   model_dataset = foi_data, 
                   grid_size = gr_size, 
                   predictors = my_predictors,
                   dependent_variable = y_var, 
                   no_trees = no_trees, 
                   min_node_size = min_node_size, 
                   x_data = x_data, 
                   y_data = y_data)
  
}

if (!CLUSTER) {
  context:::stop_parallel_cluster()
}


# ---------------------------------------- save model


res <- RF_fit$results()

model_obj_lst <- lapply(res, "[[", "obj")

write_out_rds(model_obj_lst, out_path, out_name)
