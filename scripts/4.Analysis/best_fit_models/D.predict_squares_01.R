# Makes a map of the square predictions

library(ggplot2)
library(colorRamps)
library(fields)

source(file.path("R", "plotting", "functions_for_plotting_raster_maps.R"))
source(file.path("R", "utility_functions.R"))


# define parameters ----------------------------------------------------------- 


parameters <- list(id = 15,
                   FOI_z_range = c(0, 0.06),
                   R0_1_z_range = c(0, 8),
                   R0_2_z_range = c(0, 4),
                   R0_3_z_range = c(0, 5))   

vars_to_average <- "transformed_3_r_wolbachia_4"

statistic <- "best"


# define variables ------------------------------------------------------------


z_range <- parameters$R0_3_z_range

model_type <- paste0("model_", parameters$id)

in_path <- file.path("output", 
                     "predictions_world", 
                     "best_fit_models",
                     model_type)

out_path <- file.path("figures", 
                      "predictions_world",
                      "best_fit_models",
                      model_type)


# pre processing -------------------------------------------------------------- 


my_col <- matlab.like(100)

mean_pred_fl_nm <- paste0(vars_to_average, ".rds")

df_long <- readRDS(file.path(in_path, mean_pred_fl_nm))

out_fl_nm <- paste0(vars_to_average, "_", statistic, ".png")


# plot ------------------------------------------------------------------------ 


quick_raster_map(pred_df = df_long, 
                 variable = vars_to_average, 
                 statistic = statistic, 
                 my_col = my_col, 
                 out_pt = out_path, 
                 out_name = out_fl_nm,
                 z_range = z_range)
