# calculate the partial depence of the model function on each explanatory variable

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

# my_h2o_ver <- "3.16.0.2"
# if(packageVersion("h2o") != my_h2o_ver) install.packages(file.path("R_sources", "h2o_3.16.0.2.tar.gz"), repos = NULL, type = "source")


# define parameters ----------------------------------------------------------- 


parameters <- list(
  dependent_variable = "FOI",
  no_predictors = 26)   

RF_mod_nm <- "RF_obj.rds"
train_dts_nm <- "train_dts.rds"
par_dep_nm <- "par_dep.rds"
var_imp_nm <- "var_imp.rds"

model_type_tag <- "_best_model_4"


# define variables ------------------------------------------------------------


model_type <- paste0(parameters$dependent_variable, model_type_tag)

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


# are you using the cluster? -------------------------------------------------- 


if (CLUSTER) {
  
  config <- didehpc::didehpc_config(template = "16Core")
  obj <- didehpc::queue_didehpc(ctx, config = config)
  
} else {
  
  context::context_load(ctx)
  #context::parallel_cluster_start(8, ctx)
  
}


# load data -------------------------------------------------------------------


predictor_rank <- read.csv(file.path("output", 
                                     "variable_selection",
                                     "stepwise",
                                     "predictor_rank.csv"),
                           stringsAsFactors = FALSE)


# pre processing --------------------------------------------------------------


my_predictors <- predictor_rank$name[1:parameters$no_predictors]


# submit one job --------------------------------------------------------------  


if(CLUSTER){
  
  pd <- obj$enqueue(
    calculate_par_dep(RF_obj_name = RF_mod_nm, 
                      tr_dts_name = train_dts_nm,
                      par_dep_name = par_dep_nm,
                      var_imp_name = var_imp_nm,
                      RF_obj_path = model_in_pt,
                      tr_dts_path = train_dts_in_pt,
                      par_dep_path = pdp_out_pt,
                      var_imp_path = v_imp_out_pt,
                      model_type = model_type,
                      variables = my_predictors))
  
} else {
  
  pd <- calculate_par_dep(RF_obj_name = RF_mod_nm, 
                          tr_dts_name = train_dts_nm,
                          par_dep_name = par_dep_nm,
                          var_imp_name = var_imp_nm,
                          RF_obj_path = model_in_pt,
                          tr_dts_path = train_dts_in_pt,
                          par_dep_path = pdp_out_pt,
                          var_imp_path = v_imp_out_pt,
                          model_type = model_type,
                          variables = my_predictors)
  
}
