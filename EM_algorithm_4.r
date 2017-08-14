# Estimate foi for each resampled square

options(didehpc.cluster = "fi--didemrchnb")

CLUSTER <- FALSE

my_resources <- c(
  file.path("R", "utility_functions.r"),
  file.path("R", "random_forest", "make_h2o_RF_predictions.r"))  

my_pkgs <- "h2o"

context::context_log_start()
ctx <- context::context_save(path = "context",
                             sources = my_resources,
                             packages = my_pkgs)


# ---------------------------------------- define parameters


aggr_dts_name <- "aggreg_pixel_level_env_vars_20km.rds"

out_fl_nm <- "All_FOI_estimates_disaggreg_20km.rds"

out_pth <- file.path("output", "EM_algorithm", "env_variables_foi")
  
  
# ---------------------------------------- are you using the cluster? 


if (CLUSTER) {
  
  obj <- didehpc::queue_didehpc(ctx)
  
} else {
  
  context::context_load(ctx)
  
}


# ---------------------------------------- load data


h2o.init()

RF_obj <- h2o.loadModel(
  file.path("output",
            "EM_algorithm",
            "model_objects",
            "all_data.rds"))

aggreg_pxl_env_var <- readRDS(
  file.path("output", 
            "EM_algorithm",
            "env_variables", 
            aggr_dts_name))

predictor_rank <- read.csv(
  file.path("output", 
            "variable_selection", 
            "metropolis_hastings", 
            "exp_1", 
            "variable_rank_final_fits_exp_1.csv"),
  stringsAsFactors = FALSE)


# ---------------------------------------- get the vector of best predictors


my_predictors <- predictor_rank$variable[1:9]


# ---------------------------------------- submit job


if (CLUSTER) {
  
  p_i <- obj$enqueue(
    make_predictions(
      mod_obj = RF_obj, 
      dataset = aggreg_pxl_env_var, 
      sel_preds = my_predictors))
  
} else {

  p_i <- make_h2o_predictions(
      mod_obj = RF_obj, 
      dataset = aggreg_pxl_env_var, 
      sel_preds = my_predictors)
  h2o.shutdown(prompt = FALSE)
}

aggreg_pxl_env_var$p_i <- p_i

write_out_rds(aggreg_pxl_env_var, out_pth, out_fl_nm)
