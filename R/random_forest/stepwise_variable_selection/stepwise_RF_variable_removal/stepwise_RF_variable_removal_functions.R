###
# Function for running sets of stepwise REMOVALs of predictors for spatially-cv RF models, 
# each set using a specific combination of systematically-varied factors
###
multi_grid_sizes_wrapper.removal <- function(ID.run, grid.size, cell.fraction, train.fraction, pseudoAbs.value, dataset, all_predictors)
{
  #ID.run, grid.size, cell.fraction, train.fraction, pseudoAbs.value
  
  #ID.run <- as.numeric(x["ID.run"])
  cat("ID run = ", ID.run, "\n")
  
  #grid.size <- as.numeric(x["grid.size"])
  cat("grid size = ", grid.size, "\n")
  
  #cell.fraction <- as.numeric(x["cell.fraction"])
  cat("cell fraction = ", cell.fraction, "\n")
  
  #train.fraction <- as.numeric(x["train.fraction"])
  cat("train fraction = ", train.fraction, "\n")
  
  #pseudoAbs.value <- as.numeric(x["pseudoAbs.value"])
  cat("pseudo absence value = ", pseudoAbs.value, "\n")
  
  dataset[dataset$type=="pseudoAbsence","FOI"] <- pseudoAbs.value
  
  # get the rank of less important predictors
  multi_steps_wrapper.removal <- multi_steps_wrapper(dataset = dataset, 
                                                     grid_size = grid.size, 
                                                     vector_of_predictors = all_predictors, 
                                                     no_steps = 11, 
                                                     no_fits = 1000, 
                                                     no_trees = 500, 
                                                     cell_fraction = cell.fraction, 
                                                     train_fraction = train.fraction, 
                                                     level_num = 1, 
                                                     ID_run = ID.run,
                                                     addition = FALSE)
  
  removed_predictors <- multi_steps_wrapper.removal$changed_predictor
  not_removed_predictor <- all_predictors[!all_predictors %in% removed_predictors] 
  not_removed_predictor_name <- names(dataset)[not_removed_predictor]
  names(not_removed_predictor) <- not_removed_predictor_name
  
  # get name for output df 
  df_name <- sprintf("predictor_importance_test_%s%s", paste("run", ID.run, sep="_"), ".rds")
  
  # save level 1 output df 
  saveRDS(multi_steps_wrapper.removal, file.path("output", "dengue_dataset", "predictor_importance_test", "removal", paste("run", ID.run, sep="_"), df_name))
  
  output <- vector("list", length = 2)
  output[[1]] <- not_removed_predictor
  output[[2]] <- multi_steps_wrapper.removal
  output
}
