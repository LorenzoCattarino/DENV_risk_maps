# Calulate the partial depence of the model function 
# on each explanatory variable,
# for each model fit.

options(didehpc.cluster = "fi--didemrchnb")

CLUSTER <- TRUE

my_resources <- c(
  file.path("R", "utility_functions.r"),
  file.path("R", "random_forest", "functions_for_partial_dependence_plots.R"))
  
my_pkgs <- c("h2o")

context::context_log_start()
ctx <- context::context_save(path = "context",
                             sources = my_resources,
                             packages = my_pkgs)


# define parameters ----------------------------------------------------------- 


model_type <- "R0_3_boot_model"

RF_mod_name <- "RF_obj_sample"

no_fits <- 200

model_in_pt <- file.path("output",
                         "EM_algorithm",
                         model_type,
                         "optimized_model_objects")

pdp_out_pt <- file.path("output",
                        "EM_algorithm",
                        model_type,
                        "partial_dependence")

v_imp_out_pt <- file.path("output",
                          "EM_algorithm",
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


# load data -------------------------------------------------------------------


predictor_rank <- read.csv(
  file.path("output", 
            "variable_selection", 
            "metropolis_hastings", 
            "exp_1", 
            "variable_rank_final_fits_exp_1.csv"),
  stringsAsFactors = FALSE)


# pre processing --------------------------------------------------------------


variables <- predictor_rank$variable[1:9]


# submit one job --------------------------------------------------------------  


# t <- obj$enqueue(
#   calculate_par_dep(seq_len(no_fits)[1],
#                     RF_mod_name = RF_mod_name,
#                     model_in_path = model_in_pt,
#                     model_type = model_type,
#                     variables = variables,
#                     out_path_1 = pdp_out_pt,
#                     out_path_2 = v_imp_out_pt))


# submit all jobs -------------------------------------------------------------


if (CLUSTER) {

  pd_tables <- queuer::qlapply(
    seq_len(no_fits),
    calculate_par_dep,
    obj,
    RF_mod_name = RF_mod_name,
    model_in_path = model_in_pt,
    model_type = model_type,
    variables = variables,
    out_path_1 = pdp_out_pt,
    out_path_2 = v_imp_out_pt)

} else {

  pd_tables <- lapply(
    seq_len(no_fits)[1],
    calculate_par_dep,
    RF_mod_name = RF_mod_name,
    model_in_path = model_in_pt,
    model_type = model_type,
    variables = variables,
    out_path_1 = pdp_out_pt,
    out_path_2 = v_imp_out_pt)

}
