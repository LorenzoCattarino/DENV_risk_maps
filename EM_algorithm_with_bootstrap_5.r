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
  dependent_variable = "FOI",
  pseudoAbs_value = -0.02,
  grid_size = 5,
  no_trees = 500,
  min_node_size = 20,
  no_samples = 200,
  EM_iter = 10,
  no_predictors = 26)   

model_type_tag <- "_boot_model_21"


# define variables ------------------------------------------------------------


no_samples <- parameters$no_samples

model_type <- paste0(parameters$dependent_variable, model_type_tag)

my_dir <- paste0("grid_size_", parameters$grid_size)

#strip_labs_2 <- gsub("([[:punct:]])|\\s+", "_", strip_labs)

diag_t_pth <- file.path("output", 
                        "EM_algorithm", 
                        "bootstrap_models", 
                        my_dir, 
                        model_type, 
                        "diagnostics")

figure_out_path <- file.path("figures", 
                             "EM_algorithm",
                             "bootstrap_models",
                             my_dir, 
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
