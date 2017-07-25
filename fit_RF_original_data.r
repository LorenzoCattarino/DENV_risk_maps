options(didehpc.cluster = "fi--didemrchnb")

CLUSTER <- FALSE

my_resources <- c(
  file.path("R", "random_forest", "wrapper_to_spatial_sampK_cv_rng3.R"),
  file.path("R", "random_forest", "spatial_sampK_cv_rng3.R"),
  file.path("R", "random_forest", "fit_h2o_random_forest_model.r"),
  file.path("R", "random_forest", "make_h2o_RF_predictions.r"),
  file.path("R", "generic_scatter_plot.r")
)

my_pkgs <- c("h2o", "weights", "ggplot2")

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

no_fits <- 1

#out_path <- file.path("output", "model_objects")

out_name <- "fit_all_data"   


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

# load datasets for predictions
adm1_dataset <- read.csv(file.path("output", "env_variables", "All_adm1_env_var.csv"))
adm2_dataset <- read.csv(file.path("output", "env_variables", "All_adm2_env_var.csv"))


# ---------------------------------------- get the vector of best predictors


my_predictors <- predictor_rank$variable[1:9]


# ---------------------------------------- pre process the original foi dataset


# set pseudo absence value
foi_data[foi_data$type == "pseudoAbsence", y_var] <- pseudoAbs_value

# assign weights
foi_data$new_weight <- all_wgt
foi_data[foi_data$type == "pseudoAbsence", "new_weight"] <- pAbs_wgt

# add an ID for each data point (used by get_training_point_positions() and get_validating_point_positions())
training_dataset <- cbind(id = seq_len(nrow(foi_data)), foi_data)


# ------------------------------------- Run job


if (CLUSTER) {
  
  RF_run <- obj$en

} else {
  
  RF_run <- spatial.cv.rf(
    preds = my_predictors, 
    y_var = y_var, 
    train_set = training_dataset, 
    no_trees = no_trees, 
    min_node_size = min_node_size, 
    my_weights = "new_weight",
    model_name = out_name)
  
}

data_to_plot <- cbind(foi_data, pred = RF_run)

generic_scatter_plot(
  df_to_plot = data_to_plot, 
  x = "FOI", 
  y = "pred", 
  my_path = ".", 
  file_name = "test.png", 
  x_axis_tag = "observations",
  y_axis_tag = "predictions", 
  ttl = NULL, 
  alpha = NULL, 
  reverse_x_axis = FALSE, 
  mirror = FALSE)

wtd.cors(data_to_plot$FOI, data_to_plot$pred, data_to_plot$new_weight)
