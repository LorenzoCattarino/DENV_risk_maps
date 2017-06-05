###
# function for running different sets of random forest models, each set with a different combination of predictors
###
predictor_combs_wrapper <- function(x, dataset, grid_size, no_fits, no_trees, cell_fraction, train_fraction)
{
  cat("Process ID = ", Sys.getpid(), "\n")
  
  combination.of.predictors <- x
  cat("combination of predictors = ", combination.of.predictors, "\n")
         
  # run a set of random forest models (with spatially-stratified cross validation) with a specific combination of predictors
  Run <- spatial.cv.rf (dataset,
                        full_dataset = NULL,
                        grid_size, 
                        combination_of_predictors = combination.of.predictors, 
                        no_fits, 
                        no_trees, 
                        cell_fraction, 
                        train_fraction)
}

###
# function for calcualting the selection frequency of predictors selected across multiple runs of a variable selection routine 
###
sorting.sel.freq <- function(all_predictors, top_predictor_number)
{
  # get selection frequency
  selection_frequency <- table(all_predictors)
  
  # sort predictors by decreasing selection frequency value
  selection_frequency_sorted <- selection_frequency[order(selection_frequency, decreasing = TRUE)]
  
  top_predictor_number <- ifelse(top_predictor_number>length(selection_frequency_sorted), 
                                 length(selection_frequency_sorted), top_predictor_number) 
  
  # get values of top predictors 
  best_predictors <- as.numeric(names(selection_frequency_sorted[1:top_predictor_number]))
  
  best_predictors
}

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
