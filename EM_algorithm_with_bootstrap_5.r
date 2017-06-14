# For each bootstrap sample, creates and saves one plot 
# for each of three diagnostics of the EM algorithm output:
# 1) pixel level sum of squares
# 2) admin unit levele sum of square
# 3) mean square error of the RF object

options(didehpc.cluster = "fi--didemrchnb")

CLUSTER <- TRUE

my_resources <- c(
  file.path("R", "random_forest", "wrapper_to_Exp_Max_algorithm.r"),
  file.path("R", "random_forest", "get_1_0_point_position.r"),
  file.path("R", "random_forest", "spatial_sampK_cv_rng3.r"),
  file.path("R", "random_forest", "fit_random_forest_model.r"),
  file.path("R", "random_forest", "make_RF_predictions.r"),
  file.path("R", "random_forest", "calculate_sum_squared_errors.r"),
  file.path("R", "random_forest", "Exp_Max_algorithm.r"),
  file.path("R", "utility_functions.r"))

my_pkgs <- c("ranger", "dplyr", "ggplot2")

context::context_log_start()
ctx <- context::context_save(path = "context",
                             sources = my_resources,
                             packages = my_pkgs)


# ---------------------------------------- rebuild the queue object?


if (CLUSTER) {
  
  obj <- didehpc::queue_didehpc(ctx)
  
}else{
  
  context::context_load(ctx)
  
}


# ---------------------------------------- define parameters


no_fits <- 1
  
diagnostic_vars <- c("RF_ms_i", "ss_i", "ss_j")

strip_labs <- c(
  "internal RF mean square error", 
  "pixel level sum of square", 
  "admin unit level sum of square") 

strip_labs <- gsub('([[:punct:]])|\\s+','_', strip_labs)

fig_file_tag <- paste0(strip_labs, ".png")

figure_out_path <- file.path("figures", 
                             "EM_algorithm", 
                             "boot_model_20km_cw", 
                             "boot_samples",
                             paste0("sample_", seq_len(no_fits)))


# ---------------------------------------- get results 


my_task_id <- "8cdb82063453ed338881e27c2554ffb4"

EM_alg_run_t <- obj$task_get(my_task_id)
#EM_alg_run_t <- obj$task_bundle_get(my_task_id)

EM_alg_run <- EM_alg_run_t$result()
#EM_alg_run <- EM_alg_run_t$results()


# ---------------------------------------- plot 


for (j in seq_len(no_fits)){
  
  my_path <- figure_out_path[j]
  
  one_data_set <- EM_alg_run[[2]] 
    
  data_to_plot <- as.data.frame(one_data_set)
  
  data_to_plot$iter <- seq_len(nrow(data_to_plot))
  
  dir.create(my_path, FALSE, TRUE)
  
  for (i in seq_along(strip_labs)){
    
    png(file.path(my_path, fig_file_tag[i]), 
        width = 5, height = 4.5, units = "in",
        res = 300)
    
    print(ggplot(data_to_plot, aes(iter, get(diagnostic_vars[i]))) +
          geom_line() +
          scale_x_continuous("Iterations") +
          scale_y_continuous(strip_labs[i]) +
          theme(axis.title.x = element_text(size = 12),
                axis.title.y = element_text(size = 12),
                axis.text.x = element_text(size = 12),
                axis.text.y = element_text(size = 12)))
    
    dev.off()
    
  }

}
