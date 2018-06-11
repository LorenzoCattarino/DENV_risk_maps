# Calulate the partial depence of the model function on each explanatory variable,
# for each model fit.

options(didehpc.cluster = "fi--didemrchnb")

CLUSTER <- TRUE

my_resources <- c(
  file.path("R", "random_forest", "partial_dependence_plots.R"),
  file.path("R", "utility_functions.R"))
  
my_pkgs <- "h2o"

context::context_log_start()
ctx <- context::context_save(path = "context",
                             sources = my_resources,
                             packages = my_pkgs)


# define parameters ----------------------------------------------------------- 


parameters <- list(
  dependent_variable = "R0_3",
  grid_size = 5,
  no_samples = 200,
  no_predictors = 9)   

RF_mod_name <- "RF_obj_sample"


# define variables ------------------------------------------------------------


no_samples <- parameters$no_samples

model_type <- paste0(parameters$dependent_variable, "_boot_model")

my_dir <- paste0("grid_size_", parameters$grid_size)

model_in_pt <- file.path("output",
                         "EM_algorithm",
                         "bootstrap_models",
                         my_dir,
                         model_type,
                         "optimized_model_objects")

train_dts_in_pt <- file.path("output",
                             "EM_algorithm",
                             "bootstrap_models",
                             my_dir,
                             model_type,
                             "training_datasets")

pdp_out_pt <- file.path("output",
                        "EM_algorithm",
                        "bootstrap_models",
                        my_dir,
                        model_type,
                        "partial_dependence")

v_imp_out_pt <- file.path("output",
                          "EM_algorithm",
                          "bootstrap_models",
                          my_dir,
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
                                     "metropolis_hastings", 
                                     "exp_1", 
                                     "variable_rank_final_fits_exp_1.csv"),
                           stringsAsFactors = FALSE)


# pre processing --------------------------------------------------------------


variables <- predictor_rank$name[1:parameters$no_predictors]


# submit one job --------------------------------------------------------------  


t <- obj$enqueue(
  calculate_par_dep(seq_len(no_samples)[1],
                    RF_mod_name = RF_mod_name,
                    model_in_path = model_in_pt,
                    train_dts_in_path = train_dts_in_pt,
                    model_type = model_type,
                    variables = variables,
                    out_path_1 = pdp_out_pt,
                    out_path_2 = v_imp_out_pt))


# submit all jobs -------------------------------------------------------------


# if (CLUSTER) {
# 
#   pd_tables <- queuer::qlapply(
#     seq_len(no_samples)[1],
#     calculate_par_dep,
#     obj,
#     RF_mod_name = RF_mod_name,
#     model_in_path = model_in_pt,
#     train_dts_in_path = train_dts_in_pt,
#     model_type = model_type,
#     variables = variables,
#     out_path_1 = pdp_out_pt,
#     out_path_2 = v_imp_out_pt)
# 
# } else {
# 
#   pd_tables <- lapply(
#     seq_len(no_samples)[1],
#     calculate_par_dep,
#     RF_mod_name = RF_mod_name,
#     model_in_path = model_in_pt,
#     train_dts_in_path = train_dts_in_pt,
#     model_type = model_type,
#     variables = variables,
#     out_path_1 = pdp_out_pt,
#     out_path_2 = v_imp_out_pt)
# 
# }
