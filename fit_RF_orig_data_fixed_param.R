options(didewin.cluster = "fi--didemrchnb")

CLUSTER <- FALSE

my_resources <- c(
file.path("R", "utility_functions.r"),
  file.path("R", "random_forest", "fit_random_forest_model.r"),
  file.path("R", "random_forest", "make_RF_predictions.r"),
  file.path("R", "random_forest", "get_1_0_point_position.r"),
  file.path("R", "random_forest", "calculate_sum_squared_errors.r"),
  file.path("R", "random_forest", "spatial_sampK_cv_rng3.r"))

my_pkgs <- c("ranger", "weights")

context::context_log_start()
ctx <- context::context_save(packages = my_pkgs,
                             sources = my_resources,
                             root = "context")


# ---------------------------------------- define parameters


dependent_variable <- "FOI"

no_trees <- 500

min_node_size <- 20

pseudoAbs_value <- -0.02

all_wgt <- 1

pAbs_wgt <- 0.25

out_path <- file.path("output", "model_objects")

out_name <- "list_of_boot_models.rds"   
          

# ---------------------------------------- Are you using the cluster?


if (CLUSTER) {
  
  config <- didewin::didewin_config(template = "24Core")
  obj <- didewin::queue_didewin(ctx, config = config)

} else {
  
  context::context_load(ctx)

}


# ---------------------------------------- Load data


# load FOI dataset
foi_data <- read.csv(
  file.path("output", 
            "dengue_dataset", 
            "All_FOI_estimates_linear_env_var.csv"),
  stringsAsFactors = FALSE)

# predicting variable rank
predictor_rank <- read.csv(
  file.path("output", 
            "dengue_dataset", 
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
foi_data[foi_data$type == "pseudoAbsence", dependent_variable] <- pseudoAbs_value

# assign weights
foi_data$new_weight <- all_wgt
foi_data[foi_data$type == "pseudoAbsence", "new_weight"] <- pAbs_wgt

# add an ID for each data point (used by get_training_point_positions() and get_validating_point_positions())
training_dataset <- cbind(id = seq_len(nrow(foi_data)), foi_data)


# ---------------------------------------- creat objects needed for run


no_data <- nrow(foi_data)

# get the position (1/0) of the points in the validating dataset
valid_point_pos <- get_validating_point_positions(no_data, training_dataset)

# get training dataset (full dataset - no bootstrap)
training_dataset <- foi_data[, c(dependent_variable, my_predictors)]

# get y values to predict 
y_data <- training_dataset[, dependent_variable]

# get x values to make predictions on
x_data <- training_dataset[, my_predictors]

my_weights <- foi_data$new_weight


# ------------------------------------- Run RF fits


if (CLUSTER) {
  
  RF_fit <- obj$enqueue(
    spatial.cv.rf(
      preds = my_predictors, 
      y_var = dependent_variable, 
      train_set = training_dataset, 
      no_trees = no_trees, 
      min_node_size = min_node_size, 
      x_data = x_data, 
      y_data = y_data, 
      valid_points = valid_point_pos,
      my_weights = my_weights))
  
}else{
  
  RF_fit <- spatial.cv.rf(
    preds = my_predictors, 
    y_var = dependent_variable, 
    train_set = training_dataset, 
    no_trees = no_trees, 
    min_node_size = min_node_size, 
    x_data = x_data, 
    y_data = y_data, 
    valid_points = valid_point_pos,
    my_weights = my_weights)
  
}

if (!CLUSTER) {
  context:::stop_parallel_cluster()
}

write_out_rds(dat = RF_fit[[1]],
              my_path = out_path,
              file_name = out_name)
