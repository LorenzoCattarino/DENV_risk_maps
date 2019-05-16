options(didewin.cluster = "fi--didemrchnb")

CLUSTER <- TRUE

original_data <- FALSE
  
my_resources <- c(
  file.path("R", "write_out_csv.R"),
  file.path("R", "utility_functions.R"),
  file.path("R", "random_forest", "wrapper_to_multi_factor_RF_fit_boot_cv.R"), 
  file.path("R", "random_forest", "grid_up_foi_dataset.R"),
  file.path("R", "random_forest", "bootstrap_foi_dataset.R"),
  file.path("R", "random_forest", "fit_random_forest_model.R"),
  file.path("R", "random_forest", "make_RF_predictions.R"),
  file.path("R", "random_forest", "get_1_0_point_position.R"),
  file.path("R", "random_forest", "calculate_sum_squared_errors.R"),
  file.path("R", "random_forest", "spatial_sampK_cv_rng3.R"),
  file.path("R", "random_forest", "wrapper_to_spatial_sampK_cv_rng3.R"))

my_pkgs <- c("ranger", "weights")


# ---------------------------------------- Are you using the cluster?


context::context_log_start()
ctx <- context::context_save(packages = my_pkgs,
                             sources = my_resources,
                             root = "context")

if(CLUSTER) {
  config <- didewin::didewin_config(template = "12and16Core")
  obj <- didewin::queue_didewin(ctx, config = config)
}else{
  context::context_load(ctx)
  context::start_parallel_cluster(8, ctx)
}


# ---------------------------------------- Load data


# load FOI dataset
if(original_data){
  
  dengue_dataset <- read.csv(
    file.path("output", 
              "dengue_dataset", 
              "All_FOI_estimates_linear_env_var.csv"),
    header = TRUE) 
  
}else{
  
  dengue_dataset <- readRDS(
    file.path("output", 
              "dengue_dataset", 
              "All_FOI_estimates_disaggreg.RDS")) 
}

# predicting variable names
all_predictors <- read.table(
  file.path("output", 
            "datasets", 
            "all_predictors.txt"), 
  header = TRUE, 
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


# ---------------------------------------- define parameters


all_tuned_parameters <- c("tree_number", "node_size", "final_model") 

tuned_parameter <- all_tuned_parameters[2]

exp_id <- 8

coord_flds <- c("latitude", "longitude")

all_wgt <- 1

pAbs_wgt <- 0.25

nruns <- 500

try_values <- seq(2, 100, 2)
 
   
# ---------------------------------------- Pre-processing 


names(dengue_dataset)[names(dengue_dataset) == "lat.grid"] <- coord_flds[1]
names(dengue_dataset)[names(dengue_dataset) == "long.grid"] <- coord_flds[2]

# assign case weights 
dengue_dataset$new.weight <- all_wgt
dengue_dataset[dengue_dataset$type == "pseudoAbsence", "new.weight"] <- pAbs_wgt

# add an ID for each data point (used by get_training_point_positions() and get_validating_point_positions())
dengue_dataset <- cbind(id = seq_len(nrow(dengue_dataset)), dengue_dataset)

# get all predictor names except land use classes
all_predictors_no_LC <- all_predictors$variable[1:26]

# get the vector of best predictors (from MH variable selection routine)
MH_selected_predictors <- predictor_rank$variable[1:9]

my_predictors <- MH_selected_predictors


# ---------------------------------------- Set up simulation framework


# get all combinations of factors for model fitting
factor_combinations <- expand.grid(exp_ID = exp_id, 
                                   tree_num = 300,
                                   node_size = try_values,
                                   grid_size = 5, 
                                   pseudo_absence_value = 0,
                                   pseudo_absence_prop = 1,
                                   runs = nruns)

# add run ID field
factor_combinations <- cbind(run_ID = seq_len(nrow(factor_combinations)), 
                             factor_combinations)

factor_combinations_list <- df_to_list (x = factor_combinations, use_names = TRUE)


# ------------------------------------- Run RF fits


# # try one job
# t <- obj$enqueue(
#   wrapper_to_multi_factor_RF_fit(
#     factor_combinations_list[[1]],
#     model_dataset = dengue_dataset,
#     predictors = my_predictors,
#     dependent_variable = "FOI",
#     ranger_out = FALSE))

if (CLUSTER) {
  
  task_b_name <- paste(tuned_parameter, exp_id, sep = "_exp_")
  
  RF_fits <- queuer::qlapply(
    factor_combinations_list, 
    wrapper_to_multi_factor_RF_fit, 
    obj,
    model_dataset = dengue_dataset, 
    predictors = my_predictors,
    dependent_variable = "FOI",
    ranger_out = FALSE,
    fit_parallel = TRUE,
    timeout = 0,
    name = task_b_name)
  
}else{
  
  system.time(RF_fits <- lapply(
    factor_combinations_list,
    wrapper_to_multi_factor_RF_fit,
    model_dataset = dengue_dataset, 
    predictors = my_predictors,
    dependent_variable = "FOI",
    ranger_out = TRUE,
    fit_parallel = TRUE))
  
}

if (!CLUSTER) {
  context:::stop_parallel_cluster()
}

# write out exp des
write_out_csv(x = factor_combinations, 
              my_path = file.path("output", 
                                  "dengue_dataset",
                                  "sensitivity_analysis",
                                  tuned_parameter), 
              file_name = "factor_combinations",
              exp_id = exp_id)
