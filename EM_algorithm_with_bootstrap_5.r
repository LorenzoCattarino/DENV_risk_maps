# For each bootstrap sample, creates and saves one plot 
# for each of three diagnostics of the EM algorithm output:
#
# 1) pixel level sum of squares
# 2) admin unit level sum of square
# 3) mean square error of the RF object

library(ggplot2)


# define parameters ----------------------------------------------------------- 


parameters <- list(
  dependent_variable = "FOI",
  pseudoAbs_value = -0.02,
  grid_size = 0.5,
  no_trees = 500,
  min_node_size = 20,
  no_samples = 200,
  EM_iter = 10,
  no_predictors = 9)   

diagnostic_vars <- c("RF_ms_i", "ss_i", "ss_j", "r_av_sqr", "r_adm")

strip_labs <- c("mean square error", 
                "pixel level sum of square", 
                "admin unit level sum of square",
                "pixel level correlation",
                "admin unit level correlation") 

names(strip_labs) <- diagnostic_vars


# define variables ------------------------------------------------------------


no_samples <- parameters$no_samples

model_type <- paste0(parameters$dependent_variable, "_boot_model")

my_dir <- paste0("grid_size_", parameters$grid_size)

#strip_labs_2 <- gsub("([[:punct:]])|\\s+", "_", strip_labs)

diag_t_pth <- file.path("output", 
                        "EM_algorithm", 
                        "bootstrap_models", 
                        my_dir, 
                        model_type, 
                        "diagnostics")

fig_file_tag <- "diagnostics.png"

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
  
  data_to_plot <- as.data.frame(one_data_set)
  
  data_to_plot$iter <- seq_len(nrow(data_to_plot))
  
  data_to_plot_long <- reshape2::melt(data_to_plot, 
                                      id.vars = "iter", 
                                      measure.vars = diagnostic_vars)
  
  dir.create(figure_out_path, FALSE, TRUE)
    
  p <- ggplot(data_to_plot_long, aes(iter, value)) +
    geom_line() +
    scale_x_continuous("Iterations", breaks = seq_len(10), labels = seq_len(10)) +
    scale_y_continuous(NULL) +
    facet_wrap(~ variable, ncol = 2, scales = "free_y", labeller = labeller(variable = strip_labs)) +
    theme(axis.title.x = element_text(size = 12),
          axis.title.y = element_text(size = 12),
          axis.text.x = element_text(size = 10),
          axis.text.y = element_text(size = 10))
  
  fig_file_tag <- paste0("sample_", j, ".png")
  
  png(file.path(figure_out_path, fig_file_tag), 
      width = 13, 
      height = 13, 
      units = "cm",
      pointsize = 12,
      res = 200)
  
  print(p)
  
  dev.off()
  
}
