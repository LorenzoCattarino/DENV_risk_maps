# Fits RF to all original foi data (using fixed RF parameters) 

options(didehpc.cluster = "fi--didemrchnb")

CLUSTER <- FALSE

my_resources <- c(
  file.path("R", "utility_functions.r"),
  file.path("R", "random_forest", "fit_h2o_random_forest_model.r"),
  file.path("R", "random_forest", "make_h2o_RF_predictions.r"),
  file.path("R", "random_forest", "spatial_sampK_cv_rng3.r"))

my_pkgs <- c("h2o")

context::context_log_start()
ctx <- context::context_save(path = "context",
                             sources = my_resources,
                             packages = my_pkgs)


# ---------------------------------------- define parameters


y_var <- "FOI"

no_trees <- 500

min_node_size <- 20

pseudoAbs_value <- 0

all_wgt <- 1

pAbs_wgt <- 0.25

out_path <- file.path("output", "EM_algorithm", "model_objects")

out_name <- "all_data.rds"   


# ---------------------------------------- Are you using the cluster?


if (CLUSTER) {
  
  #config <- didewin::didewin_config(template = "24Core")
  obj <- didehpc::queue_didehpc(ctx)

} else {
  
  context::context_load(ctx)

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


# set pseudo absence value
foi_data[foi_data$type == "pseudoAbsence", y_var] <- pseudoAbs_value

# assign weights
foi_data$new_weight <- all_wgt
foi_data[foi_data$type == "pseudoAbsence", "new_weight"] <- pAbs_wgt


# ---------------------------------------- create objects needed for run


# get training dataset (full dataset - no bootstrap)
training_dataset <- foi_data[, c(y_var, my_predictors, "new_weight")]


# ------------------------------------- # run job


if (CLUSTER) {
  
  RF_fit <- obj$enqueue(
    spatial.cv.rf(
      preds = my_predictors, 
      y_var = y_var, 
      train_set = training_dataset, 
      no_trees = no_trees, 
      min_node_size = min_node_size, 
      my_weights = "new_weight",
      model_name = out_name,
      model_path = out_path))
  
} else {
  
  RF_fit <- spatial.cv.rf(
    preds = my_predictors, 
    y_var = y_var, 
    train_set = training_dataset, 
    no_trees = no_trees, 
    min_node_size = min_node_size, 
    my_weights = "new_weight",
    model_name = out_name,
    model_path = out_path)
  
}
