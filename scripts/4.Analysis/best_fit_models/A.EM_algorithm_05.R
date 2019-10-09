# Creates and saves one plot for each of three diagnostics 
# of the EM algorithm output:
#
# 1) pixel level sum of squares
# 2) admin unit levele sum of square
# 3) mean square error of the RF object

library(ggplot2)

source(file.path("R", "plotting", "plot_EM_diagnostics.R"))
source(file.path("R", "utility_functions.R"))


# define parameters ----------------------------------------------------------- 


parameters <- list(id = 1,
                   dependent_variable = "FOI")   


# define variables ------------------------------------------------------------


model_id <- parameters$id

model_type <- paste0("model_", model_id)

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


# save data plot --------------------------------------------------------------


diagnostic_vars <- c("RF_ms_i", "ss_i", "ss_j", "r_av_sqr", "r_adm")

data_to_plot_sub <- data_to_plot[, diagnostic_vars]
  
data_to_save <- as.data.frame(data_to_plot_sub)

strip_labs <- c("mean square error", 
                "1/6 degree sum of square", 
                "admin unit sum of square",
                "1/6 degree correlation",
                "admin unit correlation") 

names(data_to_save) <- strip_labs

data_to_save$iter <- seq_len(nrow(data_to_save))

data_to_save_long <- reshape2::melt(data_to_save, 
                                    id.vars = "iter", 
                                    measure.vars = strip_labs)

write_out_csv(data_to_save_long, diag_t_pth, "diagnostic_data_plot.csv", row.names = FALSE)
  