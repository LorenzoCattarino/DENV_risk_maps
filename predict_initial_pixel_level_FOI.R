options(didewin.cluster = "fi--didemrchnb")

CLUSTER <- FALSE

my_resources <- c(
  file.path("R", "utility_functions.R"),
  file.path("R", "prepare_datasets", "remove_NA_rows.R"),
  file.path("R", "random_forest", "make_RF_predictions.R"))  

my_pkgs <- c("ranger")

context::context_log_start()
ctx <- context::context_save(packages = my_pkgs,
                             sources = my_resources,
                             root = "context")


# ---------------------------------------- define parameters


aggr_dts_name <- "aggreg_pixel_level_env_vars_20km.RDS"

out_fl_nm <- "All_FOI_estimates_disaggreg_20km.RDS"


# ---------------------------------------- are you using the cluster? 


if (CLUSTER) {
  
  obj <- didewin::queue_didewin(ctx)
  
} else {
  
  context::context_load(ctx)
  
}


# ---------------------------------------- load data


RF_obj <- readRDS(
  file.path("output",
            "model_objects",
            "fit_all_data_model.RDS"))

aggreg_pxl_env_var <- readRDS(
  file.path("output", 
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


# ---------------------------------------- pre process the prediction dataset


aggreg_pxl_env_var[, my_predictors][aggreg_pxl_env_var[, my_predictors] == 0] <- NA

#remove records with at least one NA predictor value
aggreg_pxl_env_var <- remove_NA_rows(aggreg_pxl_env_var, my_predictors)


# ---------------------------------------- submit job


if(CLUSTER){
  
  p_i <- obj$enqueue(
    make_predictions(
      mod_obj = RF_obj, 
      dataset = aggreg_pxl_env_var, 
      sel_preds = my_predictors))
  
}else{
  
  p_i <- make_predictions(
      mod_obj = RF_obj, 
      dataset = aggreg_pxl_env_var, 
      sel_preds = my_predictors)
  
}

aggreg_pxl_env_var$p_i <- p_i

write_out_rds(aggreg_pxl_env_var, "output", out_fl_nm)
