###
# function for plotting results of stepwise variable addition   
###
plotting_results.addition <- function(data_set_1, my_data, grid_size, run_id)
{
  # plot title
  predictor_importance_test.plot_title <- sprintf("Run %s (grid size = %s)", run_id, grid_size)
  
  no.steps_level_1 <- nrow(my_data[[1]])
  no.steps_level_2 <- nrow(my_data[[2]])
  
  selected_predictors_level_1 <- my_data[[1]]$added_predictor
  selected_predictors_level_2 <- my_data[[2]]$added_predictor
  
  max_value.y_interval_level_1 <- ceiling(max(my_data[[1]]$mean_rmse)/.05)*.05 # to the nearest .05
  min_value.y_interval_level_1 <- floor(min(my_data[[1]]$mean_rmse)/.05)*.05

  max_value.y_interval_level_2 <- ceiling(max(my_data[[2]]$mean_rmse)/.05)*.05 # to the nearest .05
  min_value.y_interval_level_2 <- floor(min(my_data[[2]]$mean_rmse)/.05)*.05
  
  increment <- 0.05  
  
  # plot level 1 
  predictor_importance_plot_level_1 <- ggplot(my_data[[1]], aes(x=Step, y=mean_rmse)) +
    ggtitle("First level of filtering") + 
    geom_point(aes(x=Step, y=mean_rmse), size=1) +
    scale_x_continuous("Added predictor", 
                       limits=c(1, no.steps_level_1), 
                       breaks=seq(1, no.steps_level_1, 1),
                       labels=colnames(data_set_1[,selected_predictors_level_1])) +
    scale_y_continuous("Mean (across 100 fits) of the RMS error (validating set)", 
                       limits=c(min_value.y_interval_level_1,max_value.y_interval_level_1), 
                       breaks=seq(min_value.y_interval_level_1,max_value.y_interval_level_1,increment)) +
    theme(axis.title.x = element_text(hjust=0.5, vjust=1),
          axis.text.x = element_text(angle = -45, hjust = 0, vjust = 1),
          axis.title.y = element_text(hjust=0.5, vjust=0),
          axis.text=element_text(size=12),
          plot.margin=unit(c(1,1,0.3,1), "cm"),
          plot.title = element_text(lineheight=1, face="bold"))
  
  predictor_importance_plot_level_2 <- ggplot(my_data[[2]], aes(x=Step, y=mean_rmse)) +
    ggtitle("Second level of filtering") + 
    geom_point(aes(x=Step, y=mean_rmse), size=1) +
    scale_x_continuous("Added predictor", 
                       limits=c(1, no.steps_level_2), 
                       breaks=seq(1, no.steps_level_2, 1),
                       labels=colnames(data_set_1[,selected_predictors_level_2])) +
    scale_y_continuous("Mean (across 1000 fits) of the RMS error (validating set)", 
                       limits=c(min_value.y_interval_level_2,max_value.y_interval_level_2), 
                       breaks=seq(min_value.y_interval_level_2,max_value.y_interval_level_2,increment)) +
    theme(axis.title.x = element_text(hjust=0.5, vjust=1),
          axis.text.x = element_text(angle = -45, hjust = 0, vjust = 1),
          axis.title.y = element_text(hjust=0.5, vjust=0),
          axis.text=element_text(size=12),
          plot.margin=unit(c(0.3,1,1,1), "cm"),
          plot.title = element_text(lineheight=1, face="bold"))

  # name of plot file 
  plot_file_name <- sprintf("plot_predictor_importance_test_%s%s%s", paste("grid_size", grid_size, sep="_"), paste("_run", run_id, sep="_"), ".pdf")
  
  # dummy
  t <- textGrob("") 
  
  final_plot <- grid.arrange(predictor_importance_plot_level_1, 
                             arrangeGrob(predictor_importance_plot_level_2,t,nrow=1),
                             ncol = 1,
                             nrow = 2,
                             top = textGrob(predictor_importance_test.plot_title, rot=0, gp=gpar(fontsize=18)))  
  ggsave(final_plot, file=file.path("figures", "graphs", "predictor_importance_test", plot_file_name), width = 12, height = 10, units = "in", dpi = 200)
}
