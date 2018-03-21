###
# function for running different sets of random forest models, each set with a different combination of predictors
###


###
# function for plotting results of stepwise variable removal   
###
plotting_results.removal <- function(df_to_plot, best_predictor, y, x)
{
  x_values <- seq_len(nrow(df_to_plot))
  y_values <- pretty(df_to_plot[,y], n=5)
  
  #selected_predictors <- df_to_plot$changed_predictor 
  #best_predictor_index <- used_predictors[!used_predictors %in% selected_predictors]
  #best_predictor_name <- colnames(main_dataset)[best_predictor_index]
  
  # plot
  stepwise_removal_plot <- ggplot(df_to_plot, aes(x = get(x), y = get(y))) +
    ggtitle("RMSE") + 
    geom_point(aes(x = get(x), y = get(y)), size=1) +
    scale_x_continuous("Removed predictor", 
                       limits=c(min(x_values), max(x_values)), 
                       breaks=x_values,
                       labels=df_to_plot$name) +
    scale_y_continuous("RMSE", 
                       limits=c(min(y_values), max(y_values)), 
                       breaks=y_values) +
    theme(axis.title.x = element_text(hjust=0.5, vjust=1),
          axis.text.x = element_text(angle = -45, hjust = 0, vjust = 1),
          axis.title.y = element_text(hjust=0.5, vjust=0),
          axis.text=element_text(size=12),
          plot.margin=unit(c(1,2.5,0.3,1), "cm"),
          plot.title = element_text(lineheight=1, face="bold")) 
    
  BL_x_limit <- ggplot_build(stepwise_removal_plot)$panel$ranges[[1]]$x.range[1]
  BR_x_limit <- ggplot_build(stepwise_removal_plot)$panel$ranges[[1]]$x.range[2]
  B_y_limit <- ggplot_build(stepwise_removal_plot)$panel$ranges[[1]]$y.range[1]
  U_y_limit <- ggplot_build(stepwise_removal_plot)$panel$ranges[[1]]$y.range[2]

  stepwise_removal_plot <- stepwise_removal_plot + annotate("text", x = BL_x_limit + 4, y = 0.0130, label = paste("Best predictor =", names(best_predictor), sep=" "), size=4)
  
  # name of plot file 
  plot_file_name <- sprintf("Stepwise_variable_removal_plot%s", ".tiff")
  
  ggsave(stepwise_removal_plot, file=file.path("figures", "dengue_dataset", "predictor_importance_test", "removal", plot_file_name), width = 8, height = 6, units = "in", dpi = 200)
}
