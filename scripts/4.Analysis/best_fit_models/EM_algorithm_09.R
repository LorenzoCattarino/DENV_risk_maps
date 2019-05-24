# calculate the partial depence of the model function on each explanatory variable

options(didehpc.cluster = "fi--dideclusthn")

CLUSTER <- FALSE

my_resources <- c(
  file.path("R", "random_forest", "partial_dependence_plots_pdp.R"),
  file.path("R", "utility_functions.R"),
  file.path("R", "create_parameter_list.R"))

my_pkgs <- c("ranger", "pdp")

context::context_log_start()
ctx <- context::context_save(path = "context",
                             sources = my_resources,
                             packages = my_pkgs)


# define parameters ----------------------------------------------------------- 


extra_prms <- list(id = 13,
                   no_predictors = 26,
                   RF_obj_name = "RF_obj.rds",
                   tr_dts_name = "train_dts.rds",
                   par_dep_nm = "par_dep.rds",
                   var_imp_nm = "var_imp.rds",
                   parallel_2 = TRUE)


# are you using the cluster? -------------------------------------------------- 


if (CLUSTER) {
  
  obj <- didehpc::queue_didehpc(ctx)
  
} else {
  
  context::context_load(ctx)
  
}


# define variables ------------------------------------------------------------


parameters <- create_parameter_list(extra_params = extra_prms)

model_type <- paste0("model_", parameters$id)

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


# load data -------------------------------------------------------------------


predictor_rank <- read.csv(file.path("output", 
                                     "variable_selection",
                                     "stepwise_v3",
                                     "predictor_rank.csv"),
                           stringsAsFactors = FALSE)


# pre processing --------------------------------------------------------------


my_predictors <- predictor_rank$name[1:parameters$no_predictors]


# submit one job --------------------------------------------------------------  


if(CLUSTER){
  
  pd <- obj$enqueue(
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
