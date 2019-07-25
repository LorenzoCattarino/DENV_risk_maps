# Calculate the partial depence of the model function on each explanatory variable,
# for each model fit.

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


extra_prms <- list(id = 4,
                   no_predictors = 26,
                   parallel_2 = FALSE) 


# are you using the cluster? -------------------------------------------------- 


if (CLUSTER) {
  
  #config <- didehpc::didehpc_config(template = "24Core")
  obj <- didehpc::queue_didehpc(ctx)
  
} else {
  
  context::context_load(ctx)
  
}


# define variables ------------------------------------------------------------


parameters <- create_parameter_list(extra_params = extra_prms)

model_type <- paste0("model_", parameters$id)

no_samples <- parameters$no_samples

model_in_pt <- file.path("output",
                         "EM_algorithm",
                         "bootstrap_models",
                         model_type,
                         "optimized_model_objects")

train_dts_in_pt <- file.path("output",
                             "EM_algorithm",
                             "bootstrap_models",
                             model_type,
                             "training_datasets")

pdp_out_pt <- file.path("output",
                        "EM_algorithm",
                        "bootstrap_models",
                        model_type,
                        "partial_dependence")

v_imp_out_pt <- file.path("output",
                          "EM_algorithm",
                          "bootstrap_models",
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


my_predictors <- predictor_rank$name[1:parameters$no_predictors]


# submit one job --------------------------------------------------------------  


# t <- obj$enqueue(
#   wrapper_over_bsamples(seq_len(no_samples)[1],
#                         parms = parameters,
#                         RF_obj_pt = model_in_pt,
#                         tr_dts_pt = train_dts_in_pt,
#                         par_dep_pt = pdp_out_pt,
#                         var_imp_pt = v_imp_out_pt,
#                         variables = my_predictors))
# 

# submit all jobs -------------------------------------------------------------


if (CLUSTER) {

  pd_tables <- queuer::qlapply(
    seq_len(no_samples),
    wrapper_over_bsamples,
    obj,
    parms = parameters,
    RF_obj_pt = model_in_pt,
    tr_dts_pt = train_dts_in_pt,
    par_dep_pt = pdp_out_pt,
    var_imp_pt = v_imp_out_pt,
    variables = my_predictors)

} else {

  pd_tables <- lapply(
    seq_len(no_samples)[1],
    wrapper_over_bsamples,
    parms = parameters,
    RF_obj_pt = model_in_pt,
    tr_dts_pt = train_dts_in_pt,
    par_dep_pt = pdp_out_pt,
    var_imp_pt = v_imp_out_pt,
    variables = my_predictors)

}
