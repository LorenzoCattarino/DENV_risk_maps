options(didewin.cluster = "fi--didemrchnb")

rebuild <- TRUE

fill_NA <- TRUE

my_resources <- c(
  file.path("R", "write_out_csv.R"),
  file.path("R", "utility_functions.R"),
  file.path("R", "random_forest", "wrapper_to_multi_factor_RF_fit_boot_cv.R"), 
  file.path("R", "random_forest", "grid_up_foi_dataset.R"),
  file.path("R", "random_forest", "bootstrap_foi_dataset.R"),
  file.path("R", "random_forest", "fit_random_forest_model.R"),
  file.path("R", "random_forest", "make_RF_predictions.R"),
  file.path("R", "random_forest", "get_1_0_point_position.R"),
  file.path("R", "random_forest", "calculate_sum_squared_errors.R"),
  file.path("R", "random_forest", "spatial_sampK_cv_rng3.R"),
  file.path("R", "random_forest", "wrapper_to_spatial_sampK_cv_rng3.R"),
  file.path("R", "random_forest", "RF_preds_vs_obs_stratified_plot.R"))

my_pkgs <- c("ranger", "weights", "reshape2")

context::context_log_start()
ctx <- context::context_save(packages = my_pkgs,
                             sources = my_resources,
                             root = "context")


# ---------------------------------------- define parameters


all_tuned_parameters <- c("tree_number", "node_size", "final_model") 

tuned_parameter <- all_tuned_parameters[2]

exp_id <- 8

task_b_name <- paste(tuned_parameter, exp_id, sep = "_exp_")


# ---------------------------------------- load data


factor_combinations <- read.csv(file.path("output", 
                                          "dengue_dataset", 
                                          "sensitivity_analysis", 
                                          tuned_parameter,
                                          paste0("factor_combinations_exp_", exp_id, ".csv")))


# ---------------------------------------- rebuild queue if needed 


if(rebuild){
  
  config <- didewin::didewin_config(template = "12and16Core")
  
  obj <- didewin::queue_didewin(ctx, config = config)  
  
  RF_fits_tb <- obj$task_bundle_get(task_b_name)
  
  RF_fits <- RF_fits_tb$results(partial = TRUE)

}


# ---------------------------------------- do your things 


diagnostics <- as.data.frame(do.call("rbind", lapply(RF_fits, "[[", 1)))

if(fill_NA){
  
  diagnostics$run_ID <- seq(6,50,1)
  
  fctr_plus_results <- merge(factor_combinations, diagnostics, by = "run_ID", all.x = TRUE)
  
}else{
  
  fctr_plus_results <- cbind(factor_combinations, diagnostics)  

}

# write out summary of results
write_out_csv(x = fctr_plus_results, 
              my_path = file.path("output", 
                                  "dengue_dataset",
                                  "sensitivity_analysis",
                                  tuned_parameter), 
              file_name = "result_summary",
              exp_id = exp_id)


# ----------------------------------------

# plot individual runs
# first choose the run for which to plot predictions vs observations

# ----------------------------------------


run_id <- 1

if(FALSE){
  
  # predictions vs observations
  lapply(RF_fits[run_id], function(x) {  
    
    # rotate df from wide to long to allow faceting
    obs_preds_df_long <- melt(
      x[[2]], 
      id.vars = c("country_code", "adm1", "y.data"),
      variable.name = "dataset")
    
    # plot
    RF.preds.vs.obs.plot.stratif.no.labels (run_id = run_id,
                                            exp_id = exp_id,
                                            diagnostics = x[[1]],
                                            predictions = obs_preds_df_long,
                                            output_folder = tuned_parameter)})
  
}
