# Fits RF to each bootstrap sample of the original data (using fixed RF parameters)
# and saves the RF model object

options(didehpc.cluster = "fi--didemrchnb")

CLUSTER <- TRUE

my_resources <- c(
  file.path("R", "utility_functions.R"),
  file.path("R", "create_parameter_list.R"),
  file.path("R", "random_forest", "fit_ranger_RF_and_make_predictions.R"),
  file.path("R", "random_forest", "wrapper_functions_for_boot_analysis.R"))

my_pkgs <- "ranger"

context::context_log_start()
ctx <- context::context_save(path = "context",
                             sources = my_resources,
                             packages = my_pkgs)


# define parameters ----------------------------------------------------------- 


extra_prms <- list(id = 29,
                   dependent_variable = "FOI",
                   no_predictors = 26,
                   ranger_threads = NULL) 


# are you using the cluster? --------------------------------------------------  


if (CLUSTER) {
  
  obj <- didehpc::queue_didehpc(ctx)
  
} else {
  
  context::context_load(ctx)
  
}


# define variables ------------------------------------------------------------


parameters <- create_parameter_list(extra_params = extra_prms)

model_type <- paste0("model_", parameters$id)

out_pt <- file.path("output", 
                    "EM_algorithm",
                    "bootstrap_models",
                    model_type, 
                    "model_objects", 
                    "boot_samples")

covariates_dir <- parameters$covariates_dir

foi_data_pth <- file.path("output", 
                          "EM_algorithm",
                          "bootstrap_models",
                          model_type, 
                          "adm_foi_data",
                          "boot_samples") 


# load data ------------------------------------------------------------------- 


predictor_rank <- read.csv(file.path("output", 
                                     "variable_selection",
                                     covariates_dir,
                                     "predictor_rank.csv"), 
                           stringsAsFactors = FALSE)


# pre processing -------------------------------------------------------------- 


my_predictors <- predictor_rank$name[1:parameters$no_predictors]

no_samples <- parameters$no_samples


# submit one job --------------------------------------------------------------  


# t <- obj$enqueue(
#   get_bsample_and_fit_RF(
#     seq_len(no_samples)[1],
#     parms = parameters,
#     foi_data_path = foi_data_pth,
#     my_preds = my_predictors,
#     out_path = out_pt))


# submit all jobs ------------------------------------------------------------- 


if (CLUSTER) {

  RF_obj <- queuer::qlapply(
    seq_len(no_samples),
    get_bsample_and_fit_RF,
    obj,
    parms = parameters,
    foi_data_path = foi_data_pth,
    my_preds = my_predictors,
    out_path = out_pt)

} else {

  RF_obj <- lapply(
    seq_len(no_samples)[1],
    get_bsample_and_fit_RF,
    parms = parameters,
    foi_data_path = foi_data_pth,
    my_preds = my_predictors,
    out_path = out_pt)

}
