options(didehpc.cluster = "fi--didemrchnb")

CLUSTER <- TRUE

my_resources <- c(
  file.path("R", "random_forest", "grid_up_foi_dataset.r"),
  file.path("R", "random_forest", "bootstrap_foi_dataset.r"),
  file.path("R", "random_forest", "make_RF_predictions.r"),
  file.path("R", "random_forest", "Exp_Max_algorithm.r"),
  file.path("R", "random_forest", "wrapper_to_Exp_Max_algorithm.r"),
  file.path("R", "utility_functions.r"))

my_pkgs <- c("ranger", "dplyr", "ggplot2")

context::context_log_start()
ctx <- context::context_save(path = "context",
                             sources = my_resources,
                             packages = my_pkgs)


# ---------------------------------------- rebuild the queue object?


if (CLUSTER) {
  
  config <- didehpc::didehpc_config(template = "24Core")
  obj <- didehpc::queue_didehpc(ctx, config = config)

}else{
  
  context::context_load(ctx)

}


# ---------------------------------------- define parameters


diagnostic_vars <- c("RF_ms_i", "ss_i", "ss_j")

strip_labs <- c(
  "internal RF mean square error", 
  "pixel level sum of square", 
  "admin unit level sum of square") 

strip_labs <- gsub('([[:punct:]])|\\s+','_', strip_labs)

fig_file_tag <- paste0(strip_labs, ".png")
  
figure_out_path <- file.path("figures", "EM_algorithm", "boot_model_20km_cw")



# ---------------------------------------- get results 


my_task_id <- "21d364a225ff60c5215b24ad468ebafe"

EM_alg_run_t <- obj$task_get(my_task_id)

EM_alg_run <- EM_alg_run_t$result()
  

# ---------------------------------------- plot 


data_to_plot <- as.data.frame(EM_alg_run[[2]])

data_to_plot$iter <- seq_len(nrow(data_to_plot))

dir.create(figure_out_path, FALSE, TRUE)

for (i in seq_along(strip_labs)){
  
  png(file.path(figure_out_path, fig_file_tag[i]), 
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
