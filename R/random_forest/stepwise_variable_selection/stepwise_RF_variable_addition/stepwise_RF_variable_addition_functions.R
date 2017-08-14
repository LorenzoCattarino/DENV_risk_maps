# The following functions are used for running a stepwise variable ADDITION routine to assess the 
# importance of env variables as predictors in a spatially cross-validated random forest model

###
# function for running sets of stepwise addition of predictors for spatially-cv RF models, each set using a specific grid size
###
multi_grid_sizes_wrapper.addition <- function(x, dataset, all_predictor_combs, factor_combs_df)
{
  # for bulk_queue 
  # ID.exp, ID.run, grid.size, cell.fraction, train.fraction, pseudoAbs.value, weights, adm, pred_tag
  
  ID.exp <- x$ID.exp
  cat("ID exp = ", ID.exp, "\n")

  ID.run <- x$ID.run
  cat("ID run = ", ID.run, "\n")
  
  grid.size <- x$grid.size
  cat("grid size = ", grid.size, "\n")
  
  cell.fraction <- x$cell.fraction
  cat("cell fraction = ", cell.fraction, "\n")
  
  train.fraction <- x$train.fraction
  cat("train fraction = ", train.fraction, "\n")
  
  pseudoAbs.value <- x$pseudoAbs.value
  cat("pseudo absence value = ", pseudoAbs.value, "\n")

  weights <- x$weights
  cat("Weights = ", weights, "\n")
  
  adm <- x$adm
  cat("full dataset ID = ", adm, "\n")
  
  pred_tag <- as.character(x$pred_tag)
  cat("tag of predictors = ", pred_tag, "\n")
  
  dataset[dataset$type == "pseudoAbsence", "FOI"] <- pseudoAbs.value
  
  if(weights == 1)
  {
    dataset$new.weight <- 1
    dataset[dataset$type == "pseudoAbsence", "new.weight"] <- 0.25
  }
  
  if(weights == 2)
  {    
    dataset$new.weight <- 1 / dataset$variance
    dataset[dataset$type == "pseudoAbsence", "new.weight"] <- mean (dataset[dataset$type != "pseudoAbsence", "new.weight"]) * 0.25
  }  

  # create folders for output tables
  dir.create(file.path("output", "dengue_dataset", "predictor_importance_test", "addition", 
                       paste("exp", ID.exp, sep = "_"), paste("run", ID.run, sep = "_")),
             FALSE, TRUE)
  
  pred_comb_index <- which(names(all_predictor_combs) == pred_tag)
  
  combination.of.predictors <- all_predictor_combs[[pred_comb_index]]
  
  # get the xx best predictors from ALL potential predictors
  spatial_cv_rf5.stepwise_variable_addition_level_1 <- multi_steps_wrapper (dataset, 
                                                                            grid_size = grid.size, 
                                                                            vector_of_predictors_chr = combination.of.predictors, 
                                                                            no_steps = 20, 
                                                                            no_fits = 50, 
                                                                            no_trees = 500, 
                                                                            cell_fraction = cell.fraction, 
                                                                            train_fraction = train.fraction, 
                                                                            level_num = 1, 
                                                                            ID_exp = ID.exp,
                                                                            ID_run = ID.run,
                                                                            addition = TRUE)
  
  # get name for level 1 output df 
  df_name_level_1 <- sprintf("predictor_importance_test_%s_%s_%s%s", paste("exp", ID.exp, sep = "_"), 
                             paste("run", ID.run, sep="_"), paste("level", 1, sep="_"), ".csv")  
  
  # save level 1 output df 
  write.table(spatial_cv_rf5.stepwise_variable_addition_level_1, file.path("output", "dengue_dataset", "predictor_importance_test", "addition", 
                                                                           paste("exp", ID.exp, sep = "_"), 
                                                                           paste("run", ID.run, sep="_"), df_name_level_1), 
              row.names = FALSE, sep = ",")
  
  # get names of best predictors
  subset_of_predictors <- spatial_cv_rf5.stepwise_variable_addition_level_1$name
  
  # get the 10 best predictors from the previous subset 
  spatial_cv_rf5.stepwise_variable_addition_level_2 <- multi_steps_wrapper (dataset, 
                                                                            grid_size = grid.size, 
                                                                            vector_of_predictors_chr = subset_of_predictors, 
                                                                            no_steps = 10, 
                                                                            no_fits = 50, 
                                                                            no_trees = 500,
                                                                            cell_fraction = cell.fraction, 
                                                                            train_fraction = train.fraction, 
                                                                            level_num = 2, 
                                                                            ID_exp = ID.exp,
                                                                            ID_run = ID.run,
                                                                            addition = TRUE)

  # get name for level 2 output df 
  df_name_level_2 <- sprintf("predictor_importance_test_%s_%s_%s%s", paste("exp", ID.exp, sep = "_"), 
                             paste("run", ID.run, sep="_"), paste("level", 2, sep="_"), ".csv")
  
  # save level 2 output df
  write.table(spatial_cv_rf5.stepwise_variable_addition_level_2, file.path("output", "dengue_dataset", "predictor_importance_test", "addition", 
                                                                           paste("exp", ID.exp, sep = "_"),
                                                                           paste("run", ID.run, sep="_"), df_name_level_2), 
              row.names = FALSE, sep=",")
  
  fact_combs_df_name <- sprintf("factor_combinations_exp_%s%s", ID.exp, ".csv")
  
  # write out the factor combination df
  
  write.table(factor_combs_df, 
              file.path("output", "dengue_dataset", "predictor_importance_test", "addition", 
                        paste("exp", ID.exp, sep = "_"), 
                        fact_combs_df_name), 
              row.names = FALSE, sep = ",")
  
  output <- vector(mode="list", length=2)
  output[[1]] <- spatial_cv_rf5.stepwise_variable_addition_level_1
  output[[2]] <- spatial_cv_rf5.stepwise_variable_addition_level_2
  output
}

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
