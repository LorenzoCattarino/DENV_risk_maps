rm(list=ls())

# load packages
library(context)
library(queuer)
library(didewin)
library(ggplot2)

#### Rebuild queue from variable removal jobs #### 
root <- "context"
ctx <- context::context_save(packages=c("ranger", "weights"),
                             sources=c(file.path("R", "random_forest", "spatial_sampK_cv_rng2_stratif_pred.R"), 
                                       file.path("R", "random_forest", "RF_variable_importance_test", "RF_variable_importance_test_common_functions.R"),
                                       file.path("R", "random_forest", "RF_variable_importance_test", "multi_steps_wrapper_DideParallel.R"),
                                       file.path("R", "random_forest", "RF_variable_importance_test", "stepwise_RF_variable_removal", "stepwise_RF_variable_removal_functions.R")), 
                             root=root)
obj <- didewin::queue_didewin(ctx)

stepwise_addition_gs_1_task_bundle_id <- "impending_wobbegongshark"
stepwise_addition_gs_5_task_bundle_id <- "rapid_lungfish"
stepwise_addition_gs_10_task_bundle_id <- "grownup_cow"

stepwise_addition_gs_1_task_bundle <- obj$task_bundle_get(stepwise_addition_gs_1_task_bundle_id)
stepwise_addition_gs_5_task_bundle <- obj$task_bundle_get(stepwise_addition_gs_5_task_bundle_id)
stepwise_addition_gs_10_task_bundle <- obj$task_bundle_get(stepwise_addition_gs_10_task_bundle_id)

# get stepwise addition results
stepwise_addition_results_gs_1 <- stepwise_addition_gs_1_task_bundle$results()[[1]][[2]]
stepwise_addition_results_gs_5 <- stepwise_addition_gs_5_task_bundle$results()[[1]][[2]]
stepwise_addition_results_gs_10 <- stepwise_addition_gs_10_task_bundle$results()[[1]][[2]]

stepwise_addition_results_gs_1_BP <- stepwise_addition_gs_1_task_bundle$results()[[1]][[1]]
stepwise_addition_results_gs_5_BP <- stepwise_addition_gs_5_task_bundle$results()[[1]][[1]]
stepwise_addition_results_gs_10_BP <- stepwise_addition_gs_10_task_bundle$results()[[1]][[1]]

# load data
dengue_dataset <- read.csv(file.path("data", "foi", "All_FOI_estimates_env_var.csv")) 

# remove NA and outliers
dengue_dataset <- dengue_dataset[!is.na(dengue_dataset$FOI),]
dengue_dataset <- dengue_dataset[!(dengue_dataset$country=="French Polynesia"|dengue_dataset$country=="Haiti"),]

# calculate weights 
#dengue_dataset$new.weight <- 1/dengue_dataset$variance
dengue_dataset$new.weight <- 1
dengue_dataset[dengue_dataset$FOI==0,"new.weight"] <- 0.25

plotting_results.removal (df_to_plot = stepwise_addition_results_gs_1, 
                          best_predictor = stepwise_addition_results_gs_1_BP, 
                          y = "rmse_valid", 
                          x = "Step")

plotting_results.removal (df_to_plot = stepwise_addition_results_gs_5, 
                          best_predictor = stepwise_addition_results_gs_5_BP, 
                          y = "rmse_valid", 
                          x = "Step")

plotting_results.removal (df_to_plot = stepwise_addition_results_gs_10, 
                          best_predictor = stepwise_addition_results_gs_10_BP, 
                          y = "rmse_valid", 
                          x = "Step")
