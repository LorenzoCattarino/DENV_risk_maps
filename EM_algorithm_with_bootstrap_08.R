# Calculate the partial depence of the model function on each explanatory variable,
# for each model fit.

options(didehpc.cluster = "fi--didemrchnb")

CLUSTER <- TRUE

my_resources <- c(
  file.path("R", "random_forest", "partial_dependence_plots_pdp.R"),
  file.path("R", "utility_functions.R"))
  
my_pkgs <- c("ranger", "pdp", "foreach")

context::context_log_start()
ctx <- context::context_save(path = "context",
                             sources = my_resources,
                             packages = my_pkgs)


# define parameters ----------------------------------------------------------- 


parameters <- list(
  id = 1,
  shape_1 = 0,
  shape_2 = 5,
  shape_3 = 1e6,
  all_wgt = 1,
  dependent_variable = "FOI",
  pseudoAbs_value = -0.02,
  grid_size = 1 / 120,
  no_predictors = 9,
  resample_grid_size = 20,
  foi_offset = 0.03,
  no_trees = 500,
  min_node_size = 20,
  no_samples = 200,
  EM_iter = 10) 


# define variables ------------------------------------------------------------


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


# are you using the cluster? -------------------------------------------------- 


if (CLUSTER) {
  
  config <- didehpc::didehpc_config(template = "20Core")
  obj <- didehpc::queue_didehpc(ctx, config = config)
  
} else {
  
  context::context_load(ctx)
  #context::parallel_cluster_start(8, ctx)
  
}

# obj$enqueue(install.packages(file.path("R_sources", "h2o_3.18.0.8.tar.gz"), repos=NULL, type="source"))$wait(Inf)
# obj$enqueue(sessionInfo())$wait(Inf)

# load data -------------------------------------------------------------------


predictor_rank <- read.csv(file.path("output", 
                                     "variable_selection",
                                     "stepwise",
                                     "predictor_rank.csv"),
                           stringsAsFactors = FALSE)


# pre processing --------------------------------------------------------------


my_predictors <- predictor_rank$name[1:parameters$no_predictors]


# submit one job --------------------------------------------------------------  


# t <- obj$enqueue(
#   wrapper_over_bsamples(seq_len(no_samples)[1],
#                         RF_obj_pt = model_in_pt,
#                         tr_dts_pt = train_dts_in_pt,
#                         par_dep_pt = pdp_out_pt,
#                         var_imp_pt = v_imp_out_pt,
#                         model_type = model_type,
#                         variables = my_predictors))


# submit all jobs -------------------------------------------------------------


if (CLUSTER) {

  pd_tables <- queuer::qlapply(
    seq_len(no_samples),
    wrapper_over_bsamples,
    obj,
    RF_obj_pt = model_in_pt,
    tr_dts_pt = train_dts_in_pt,
    par_dep_pt = pdp_out_pt,
    var_imp_pt = v_imp_out_pt,
    model_type = model_type,
    variables = my_predictors)

} else {

  pd_tables <- lapply(
    seq_len(no_samples)[1],
    wrapper_over_bsamples,
    RF_obj_pt = model_in_pt,
    tr_dts_pt = train_dts_in_pt,
    par_dep_pt = pdp_out_pt,
    var_imp_pt = v_imp_out_pt,
    model_type = model_type,
    variables = my_predictors)

}
