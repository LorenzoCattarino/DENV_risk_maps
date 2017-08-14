# Estimates foi for each resampled square, of each 20km resolution bootstrap sample

options(didehpc.cluster = "fi--didemrchnb")

CLUSTER <- TRUE

my_resources <- c(
  file.path("R", "random_forest", "load_predict_and_save.r"),
  file.path("R", "random_forest", "make_h2o_RF_predictions.r"),
  file.path("R", "utility_functions.r"))

my_pkgs <- "h2o"

context::context_log_start()
ctx <- context::context_save(path = "context",
                             sources = my_resources,
                             packages = my_pkgs)


# ---------------------------------------- define parameters


no_fits <- 50

RF_obj_path <- file.path("output", "EM_algorithm", "model_objects", "boot_samples")


# ---------------------------------------- are you using the cluster? 


if (CLUSTER) {
  
  config <- didehpc::didehpc_config(template = "12and16Core")
  obj <- didehpc::queue_didehpc(ctx, config = config)
  
} else {
  
  context::context_load(ctx)
  
}

# ---------------------------------------- load data


predictor_rank <- read.csv(
  file.path("output", 
            "variable_selection", 
            "metropolis_hastings", 
            "exp_1", 
            "variable_rank_final_fits_exp_1.csv"),
  stringsAsFactors = FALSE)


# ---------------------------------------- pre processing


my_predictors <- predictor_rank$variable[1:9]


# ---------------------------------------- submit one job 


# t <- obj$enqueue(
#   load_predict_and_save(
#     seq_len(no_fits),
#     RF_obj_path = RF_obj_path, 
#     my_preds = my_predictors, 
#     no_fits = no_fits))


# ---------------------------------------- submit all jobs


if (CLUSTER) {

  initial_square_preds <- queuer::qlapply(
    seq_len(no_fits),
    load_predict_and_save,
    obj,
    RF_obj_path = RF_obj_path,
    my_preds = my_predictors,
    no_fits = no_fits)

}else{

  initial_square_preds <- lapply(
    seq_len(no_fits)[1],
    load_predict_and_save,
    RF_obj_path = RF_obj_path,
    my_preds = my_predictors,
    no_fits = no_fits)

}
