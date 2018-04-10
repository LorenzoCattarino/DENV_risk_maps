# Estimate foi for each 20 km square of the dataset disaggregated from the entire original foi dataset

options(didehpc.cluster = "fi--didemrchnb")

my_resources <- c(
  file.path("R", "utility_functions.r"),
  file.path("R", "random_forest", "fit_h2o_RF_and_make_predictions.r"))  

my_pkgs <- "h2o"

context::context_log_start()
ctx <- context::context_save(path = "context",
                             sources = my_resources,
                             packages = my_pkgs)


# define parameters ----------------------------------------------------------- 


var_to_fit <- "FOI"

number_of_predictors <- 9

aggr_dts_name <- "env_vars_20km.rds"

out_fl_nm <- "covariates_and_foi_20km.rds"

out_pth <- file.path("output", 
                     "EM_algorithm", 
                     "best_fit_models",
                     paste0("env_variables_", var_to_fit, "_fit"))
  
  
# start up -------------------------------------------------------------------- 


context::context_load(ctx)


# load data -------------------------------------------------------------------


h2o.init()

RF_obj <- h2o.loadModel(file.path("output",
                                  "EM_algorithm",
                                  "best_fit_models",
                                  paste0("model_objects_", var_to_fit, "_fit"),
                                  "all_data.rds"))

aggreg_pxl_env_var <- readRDS(file.path("output", 
                                        "EM_algorithm",
                                        "best_fit_models",
                                        "env_variables", 
                                        aggr_dts_name))

predictor_rank <- read.csv(file.path("output", 
                                     "variable_selection", 
                                     "metropolis_hastings", 
                                     "exp_1", 
                                     "variable_rank_final_fits_exp_1.csv"),
                           stringsAsFactors = FALSE)


# pre processing --------------------------------------------------------------


my_predictors <- predictor_rank$name[1:number_of_predictors]


# submit job ------------------------------------------------------------------ 


p_i <- make_h2o_predictions(
  mod_obj = RF_obj, 
  dataset = aggreg_pxl_env_var, 
  sel_preds = my_predictors)

h2o.shutdown(prompt = FALSE)

aggreg_pxl_env_var$p_i <- p_i

write_out_rds(aggreg_pxl_env_var, out_pth, out_fl_nm)
