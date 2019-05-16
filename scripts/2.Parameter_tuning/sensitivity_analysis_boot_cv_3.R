# load packages
library(dplyr)
library(reshape2)
library(ggplot2)

# load functions 
source(file.path("R", "generic_scatter_plot.R"))


# ---------------------------------------- define parameters


all_tuned_parameters <- c("tree_number", "node_size", "final_model") 
tuned_parameter <- all_tuned_parameters[2]
exp_id <- 8
y_to_plot <- "av_obj_size"
x_to_plot <- tuned_parameter
x_axis_label <- "min node size"
y_axis_label <- "average ranger size"
plot_file_name <- paste0(y_to_plot, "_", "vs", "_", x_to_plot)
plot_title <- ""


# ---------------------------------------- load data 


fctr_plus_results <- read.csv(
  file.path("output", 
            "dengue_dataset", 
            "sensitivity_analysis", 
            tuned_parameter,
            paste0("result_summary_exp_", exp_id, ".csv")))  


# ---------------------------------------- plotting


# scatter plot of tuned parameter
generic_scatter_plot(df_to_plot = fctr_plus_results, 
                     y = y_to_plot,
                     x = x_to_plot, 
                     output_folder = tuned_parameter,
                     file_name = plot_file_name,
                     x_axis_tag = x_axis_label,
                     y_axis_tag = y_axis_label,
                     ttl = plot_title,
                     alpha = NULL,
                     reverse_x_axis = TRUE)
