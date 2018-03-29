# For each bootstrap sample, creates and saves one plot 
# for each of three diagnostics of the EM algorithm output:
#
# 1) pixel level sum of squares
# 2) admin unit level sum of square
# 3) mean square error of the RF object

library(ggplot2)


# define parameters ----------------------------------------------------------- 


parameters <- list(
  grid_size = 1,
  resample_grid_size = 20,
  no_trees = 500,
  min_node_size = 20,
  pseudoAbs_value = -0.02,
  all_wgt = 1,
  wgt_limits = c(1, 500),
  no_samples = 200,
  EM_iter = 10,
  no_predictors = 9)   

var_to_fit <- "FOI"

diagnostic_vars <- c("RF_ms_i", "ss_i", "ss_j")

strip_labs <- c("internal RF mean square error", 
                "pixel level sum of square", 
                "admin unit level sum of square") 


# define variables ------------------------------------------------------------


no_samples <- parameters$no_samples

model_type <- paste0(var_to_fit, "_boot_model")

my_dir <- paste0("grid_size_", parameters$grid_size)

strip_labs <- gsub("([[:punct:]])|\\s+", "_", strip_labs)

diag_t_pth <- file.path("output", 
                        "EM_algorithm", 
                        "bootstrap_models", 
                        my_dir, 
                        model_type, 
                        "diagnostics")

fig_file_tag <- paste0(strip_labs, ".png")

figure_out_path <- file.path("figures", 
                             "EM_algorithm",
                             my_dir, 
                             model_type, 
                             "diagnostics",
                             paste0("sample_", seq_len(no_samples)))


# get results ----------------------------------------------------------------- 


fi <- list.files(diag_t_pth, pattern = ".*.rds", full.names = TRUE)

EM_alg_run <- lapply(fi, readRDS) 


# plot ------------------------------------------------------------------------  


for (j in seq_len(no_samples)){
  
  my_path <- figure_out_path[j]
  
  one_data_set <- EM_alg_run[[j]]
  
  data_to_plot <- as.data.frame(one_data_set)
  
  data_to_plot$iter <- seq_len(nrow(data_to_plot))
  
  dir.create(my_path, FALSE, TRUE)
  
  for (i in seq_along(strip_labs)){
    
    p <- ggplot(data_to_plot, aes(iter, get(diagnostic_vars[i]))) +
      geom_line() +
      scale_x_continuous("Iterations") +
      scale_y_continuous(strip_labs[i]) +
      theme(axis.title.x = element_text(size = 12),
            axis.title.y = element_text(size = 12),
            axis.text.x = element_text(size = 12),
            axis.text.y = element_text(size = 12))
    
    png(file.path(my_path, fig_file_tag[i]), 
        width = 5, 
        height = 4.5, 
        units = "in",
        pointsize = 12,
        res = 200)
    
    print(p)
    
    dev.off()
    
  }
  
}
