# For each bootstrap sample, creates and saves one plot 
# for each of three diagnostics of the EM algorithm output:
#
# 1) pixel level sum of squares
# 2) admin unit level sum of square
# 3) mean square error of the RF object

library(ggplot2)

source(file.path("R", "plotting", "plot_EM_diagnostics.R"))


# define parameters ----------------------------------------------------------- 


parameters <- list(
  id = 1,
  shape_1 = 0,
  shape_2 = 5,
  shape_3 = 1e6,
  all_wgt = 1,
  dependent_variable = "FOI",
  pseudoAbs_value = -0.02,
  grid_size = 1 / 120,
  no_predictors = 9,
  resample_grid_size = 20,
  foi_offset = 0.03,
  no_trees = 500,
  min_node_size = 20,
  no_samples = 50,
  EM_iter = 10) 


# define variables ------------------------------------------------------------


model_type <- paste0("model_", parameters$id)

no_samples <- parameters$no_samples

#strip_labs_2 <- gsub("([[:punct:]])|\\s+", "_", strip_labs)

diag_t_pth <- file.path("output", 
                        "EM_algorithm", 
                        "bootstrap_models", 
                        model_type, 
                        "diagnostics")

figure_out_path <- file.path("figures", 
                             "EM_algorithm",
                             "bootstrap_models",
                             model_type, 
                             "diagnostics")


# get results ----------------------------------------------------------------- 


fi <- list.files(diag_t_pth, pattern = ".*.rds", full.names = TRUE)

EM_alg_run <- lapply(fi, readRDS) 


# plot ------------------------------------------------------------------------  


for (j in seq_len(no_samples)){
  
  one_data_set <- EM_alg_run[[j]]
  
  out_fl_name <- paste0("sample_", j, ".png")
  
  plot_EM_diagnostics(one_data_set, figure_out_path, out_fl_name)

}
