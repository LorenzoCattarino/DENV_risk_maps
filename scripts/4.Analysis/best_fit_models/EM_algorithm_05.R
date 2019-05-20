# Creates and saves one plot for each of three diagnostics 
# of the EM algorithm output:
#
# 1) pixel level sum of squares
# 2) admin unit levele sum of square
# 3) mean square error of the RF object

library(ggplot2)

source(file.path("R", "plotting", "plot_EM_diagnostics.R"))


# define parameters ----------------------------------------------------------- 


parameters <- list(id = 13,
                   dependent_variable = "Z")   


# define variables ------------------------------------------------------------

model_id <- parameters$id

model_type <- paste0("model_", model_id)

# strip_labs <- gsub('([[:punct:]])|\\s+','_', strip_labs)

diag_t_pth <- file.path("output", 
                        "EM_algorithm", 
                        "best_fit_models",
                        model_type, 
                        "diagnostics")

fig_file_tag <- "diagnostics.png"
  
figure_out_path <- file.path("figures", 
                             "EM_algorithm", 
                             "best_fit_models",
                             model_type, 
                             "diagnostics")


# load data ------------------------------------------------------------------- 


data_to_plot <- readRDS(file.path(diag_t_pth, "diagno_table.rds"))


# plot ------------------------------------------------------------------------


plot_EM_diagnostics(data_to_plot, figure_out_path, fig_file_tag)
