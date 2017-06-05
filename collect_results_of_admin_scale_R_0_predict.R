rm(list = ls())

# load packages
library(context)
library(queuer)
library(didewin)

# load data
factor_combinations <- read.csv(file.path("output", "dengue_dataset", "predictions", 
                                          "exp_5", "factor_combinations_exp_5.csv"))

# Rebuild the queue 
context::context_log_start()
didewin::didewin_config_global(cluster="fi--didemrchnb")
root <- "context"
ctx <- context::context_save(packages=c("ranger", "weights", "reshape2", "maptools", "colorRamps"),
                             sources=c(file.path("R", "random_forest", "generic_multi_factor_wrapper.R"),
                                       file.path("R", "random_forest", "spatial_sampK_cv_rng2_stratif_pred.R"),
                                       file.path("R", "random_forest", "RF_preds_vs_obs_stratified_plot.R"),
                                       file.path("R", "random_forest", "map_admin_foi.R")),
                             root=root)
obj <- didewin::queue_didewin(ctx)

task_bundle_id <- "wellphilosophised_grub"
experiment <- obj$task_bundle_get(task_bundle_id)
experiment_results <- experiment$results()

# Get the fit diagnostics 
diagnostics <- as.data.frame(do.call("rbind", lapply(experiment_results, "[[", 1)))

# Get the max predicted value
all_admin_predictions <- lapply(experiment_results, "[[", 3)
all_max_pred_values <- sapply(all_admin_predictions, function(x) max(x$mean_pred))

fctr_plus_results <- cbind(factor_combinations, diagnostics, max_pred = all_max_pred_values)

write.table(fctr_plus_results, 
            file.path("output", "dengue_dataset", "predictions", 
                      paste("exp", 5, sep = "_"), 
                      "summary_table_exp_5.csv"), 
            row.names = FALSE, sep = ",")
