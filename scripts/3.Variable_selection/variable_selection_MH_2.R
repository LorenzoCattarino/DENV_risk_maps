rm(list = ls())

# Load packages
library(context)
library(queuer)
library(didewin)

# load functions 
source(file.path("R", "random_forest", "write_out_csv.R"))


# ---------------------------------------- define variables 


exp_id <- 1

analysis_tag <- "variable_selection_MH"

bootstrap_runs <- 200

Niter <- 100000

task_b_name <- paste(analysis_tag, exp_id, sep = "_exp_")

cut_off <- Niter * 0.3

start_finals <- Niter - (cut_off - 1)

start_bests <- 1

top_ones <- 10

start_points <- c(start_finals, start_bests)

end_points <- c(Niter, top_ones)

set_names <- c("final_fits", "best_fits")

rebuild <- TRUE


# ---------------------------------------- load data


# predicting variable names
all_predictors <- read.table(file.path("output", 
                                       "datasets", 
                                       "all_predictors.txt"), 
                             header = TRUE, 
                             stringsAsFactors = FALSE)


# ---------------------------------------- load in results


if(rebuild){
  
  my_resources <- c(
    file.path("R", "convert_df_to_list.R"),
    file.path("R", "random_forest", "wrapper_to_multi_factor_MH_var_sel.R"),
    file.path("R", "random_forest", "grid_up_foi_dataset.R"),
    file.path("R", "random_forest", "bootstrap_foi_dataset.R"),
    file.path("R", "random_forest", "fit_random_forest_model.R"),
    file.path("R", "random_forest", "get_1_0_point_position.R"),
    file.path("R", "random_forest", "calculate_sum_squared_errors.R"),
    file.path("R", "random_forest", "spatial_sampK_cv_rng3.R"),
    file.path("R", "random_forest", "plot_MH_var_sel_outputs.R"),
    file.path("R", "random_forest", "write_out_rds.R"))
  
  my_pkgs <- c("ranger", "weights", "gridExtra")
  
  my_workdir <- "Q:/dengue_risk_mapping"
  
  didewin::didewin_config_global(cluster = "fi--didemrchnb", workdir = my_workdir)
  
  root <- file.path(my_workdir, "context")
  
  ctx <- context::context_save(packages = my_pkgs,
                               sources = my_resources,
                               root = root)
  
  obj <- didewin::queue_didewin(ctx, sync = "R")
  
  chains <- obj$task_bundle_get(task_b_name)
  
  chain_results <- chains$results()
  
}else{

  chain_results <- vector("list", length = bootstrap_runs)
  
  for (i in seq_len(bootstrap_runs))
  {
    run_tag <- sprintf("chain_run_%s%s", i, ".rds")
    
    chain_results[[i]] <- readRDS(file.path("output",
                                            "dengue_dataset",
                                            "variable_selection",
                                            "metropolis_hastings",
                                            paste("exp", exp_id, sep = "_"),
                                            run_tag))
  }

  # acc_rates <- sapply(chain_results, function(x) {mean(x$accept)} )
  # acc_rates_df <- data.frame(acceptance_rate = acc_rates)
  # 
  # ggplot(acc_rates_df, aes(acceptance_rate)) +
  #   geom_histogram() +
  #   scale_y_continuous("Frequency") +
  #   scale_x_continuous("Acceptance rates")
  # 
  # ggsave(file.path("figures", "dengue_dataset", "predictor_importance",
  #                  "metropolis_hastings", paste("exp", exp_id, sep = "_"),
  #                  "acceptance_rates_freq_distr.pdf"))
  
}


# ---------------------------------------- post processing


# get the unique values of OF obtained along the chain 
unique_OF_values <- lapply(chain_results, function(x) {x[!duplicated(x[, "cur.OF_after"]), ]})

# sort the sets of predictors in ascending order of OF 
sorted_unique_OF_values <- lapply(unique_OF_values, function(x) {x[order(x[, "cur.OF_after"], decreasing = FALSE), ]})

# get all predictor names except land use classes
all_predictors_no_LC <- all_predictors$variable[1:26]


# =================================================================

#               extract outputs of interest from chains

# =================================================================


for (i in 1:length(set_names)){
  
  a <- start_points[i]
    
  b <- end_points[i]
  
  # calculate the selection frequency of each predictor in each run
  sel_freq <- sapply(chain_results, function(x) {colMeans(x[a:b, all_predictors_no_LC])})
  
  # take the mean of the selection frequency of each predictor across all runs
  mean_sel_freq <- rowMeans(sel_freq)

  # calculate the standard deviation of the mean sel freq 
  sd_of_mean_sel_freq <- apply(sel_freq, 1, sd)
  
  # extract the number of selected predictors 
  no_sel_vars <- lapply(chain_results, function(x) {x[a:b, "cur.n.sel"]})
  
  var_rank <- data.frame(name = names(mean_sel_freq), 
                         mean_sf = mean_sel_freq,
                         sd_mean_sf = sd_of_mean_sel_freq)
  
  sorted_var_rank <- var_rank[order(var_rank$mean_sf, decreasing = TRUE), ]
  
  sorted_var_rank <- cbind(rank = seq_len(length(mean_sel_freq)), sorted_var_rank)
                 
  all_no_sel_vars <- data.frame(no_sel_vars = unlist(no_sel_vars))
  
  table_file_name <- paste("variable_rank", set_names[i], sep = "_") 
  
  plot_file_name <- paste0("sel_var_num_freq_distr", "_", set_names[i], ".tiff")
  
  
  # -------------------------------------- write out 
  
  
  write_out_csv(x = sorted_var_rank, 
                my_path = file.path("output", 
                                    "dengue_dataset", 
                                    "variable_selection",
                                    "metropolis_hastings"),
                file_name = table_file_name,
                exp_id = exp_id)
  
  ggplot(all_no_sel_vars, aes(no_sel_vars)) +
    geom_histogram() + 
    scale_y_continuous("Frequency") +
    scale_x_continuous("Number of selected variables")
  
  ggsave(file.path("figures", 
                   "dengue_dataset", 
                   "variable_selection", 
                   "metropolis_hastings", 
                   paste("exp", exp_id, sep = "_"),
                   plot_file_name),
         height = 4,
         dpi = 300)

}
