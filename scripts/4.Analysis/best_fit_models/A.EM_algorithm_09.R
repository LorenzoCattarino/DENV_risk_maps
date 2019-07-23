# calculate the partial depence of the model function on each explanatory variable

options(didehpc.cluster = "fi--didemrchnb")

CLUSTER <- TRUE

my_resources <- c(
  file.path("R", "utility_functions.R"),
  file.path("R", "create_parameter_list.R"),
  file.path("R", "random_forest", "partial_dependence_plots_pdp.R"))

my_pkgs <- c("ranger", "pdp", "foreach")

context::context_log_start()
ctx <- context::context_save(path = "context",
                             sources = my_resources,
                             packages = my_pkgs)


# define parameters ----------------------------------------------------------- 


extra_prms <- list(id = 2,
                   no_predictors = 26,
                   RF_obj_name = "RF_obj.rds",
                   tr_dts_name = "train_dts.rds",
                   par_dep_name = "par_dep.rds",
                   var_imp_name = "var_imp.rds",
                   parallel_2 = TRUE)


# are you using the cluster? -------------------------------------------------- 


if (CLUSTER) {
  
  config <- didehpc::didehpc_config(template = "24Core")
  obj <- didehpc::queue_didehpc(ctx, config = config)
  
} else {
  
  context::context_load(ctx)
  context::parallel_cluster_start(8, ctx)
}


# define variables ------------------------------------------------------------


parameters <- create_parameter_list(extra_params = extra_prms)

model_type <- paste0("model_", parameters$id)

number_of_predictors <- parameters$no_predictors

model_in_pt <- file.path("output",
                         "EM_algorithm",
                         "best_fit_models",
                         model_type,
                         "optimized_model_objects")

train_dts_in_pt <- file.path("output",
                             "EM_algorithm",
                             "best_fit_models",
                             model_type,
                             "training_datasets")

pdp_out_pt <- file.path("output",
                        "EM_algorithm",
                        "best_fit_models",
                        model_type,
                        "partial_dependence")

v_imp_out_pt <- file.path("output",
                          "EM_algorithm",
                          "best_fit_models",
                          model_type,
                          "variable_importance")

covariates_dir <- parameters$covariates_dir


# load data -------------------------------------------------------------------


predictor_rank <- read.csv(file.path("output", 
                                     "variable_selection",
                                     covariates_dir,
                                     "predictor_rank.csv"),
                           stringsAsFactors = FALSE)


# pre processing --------------------------------------------------------------


my_predictors <- predictor_rank$name[1:number_of_predictors]


# submit one job --------------------------------------------------------------  


if(CLUSTER){
  
  pd_2 <- obj$enqueue(
    calculate_par_dep(parms = parameters,
                      RF_obj_path = model_in_pt,
                      tr_dts_path = train_dts_in_pt,
                      par_dep_path = pdp_out_pt,
                      var_imp_path = v_imp_out_pt,
                      variables = my_predictors))
  
} else {
  
  pd <- calculate_par_dep(parms = parameters,
                          RF_obj_path = model_in_pt,
                          tr_dts_path = train_dts_in_pt,
                          par_dep_path = pdp_out_pt,
                          var_imp_path = v_imp_out_pt,
                          variables = my_predictors)
  
}
