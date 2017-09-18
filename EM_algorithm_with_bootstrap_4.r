# Fits RF to each bootstrap sample of the original data (using fixed RF parameters)
# and saves the RF model object

options(didehpc.cluster = "fi--didemrchnb")

CLUSTER <- TRUE

my_resources <- c(
  file.path("R", "random_forest", "get_boot_sample_and_fit_RF.r"),
  file.path("R", "random_forest", "fit_h2o_random_forest_model.r"),
  file.path("R", "utility_functions.r"))

my_pkgs <- "h2o"

context::context_log_start()
ctx <- context::context_save(path = "context",
                             sources = my_resources,
                             packages = my_pkgs)


# ---------------------------------------- define parameters


no_fits <- 50

pseudoAbsence_value <- -0.02
  
dependent_variable <- "o_j"

no_trees <- 500

min_node_size <- 20

out_pt <- file.path("output", "EM_algorithm", "model_objects", "boot_samples")


# ---------------------------------------- are you using the cluster? 


if (CLUSTER) {
  
  config <- didehpc::didehpc_config(template = "12and16Core")
  obj <- didehpc::queue_didehpc(ctx, config = config)
  
} else {
  
  context::context_load(ctx)

}


# ---------------------------------------- load data


boot_samples <- readRDS(
  file.path("output",
            "EM_algorithm",
            "boot_samples",
            "bootstrap_samples.rds"))
  
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
#   get_boot_sample_and_fit_RF(
#     seq_len(no_fits)[1],
#     boot_ls = boot_samples,
#     my_preds = my_predictors,
#     y_var = dependent_variable,
#     no_trees = no_trees,
#     min_node_size = min_node_size,
#     out_path = out_pt,
#     psAb_val = pseudoAbsence_value))


# ---------------------------------------- submit all jobs


if (CLUSTER) {

  RF_obj <- queuer::qlapply(
    seq_len(no_fits),
    get_boot_sample_and_fit_RF,
    obj,
    boot_ls = boot_samples,
    my_preds = my_predictors,
    y_var = dependent_variable,
    no_trees = no_trees,
    min_node_size = min_node_size,
    out_path = out_pt,
    psAb_val = pseudoAbsence_value)
  
} else {

  RF_obj <- lapply(
    seq_len(no_fits),
    get_boot_sample_and_fit_RF,
    boot_ls = boot_samples,
    my_preds = my_predictors,
    y_var = dependent_variable,
    no_trees = no_trees,
    min_node_size = min_node_size,
    out_path = out_pt,
    psAb_val = pseudoAbsence_value)

}
