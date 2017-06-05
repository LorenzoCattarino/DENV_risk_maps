RF_multi_factor_wrapper <- function (
  
  x, list_of_data, all_predictor_combs, dependent_variable, output_folder, 
  plot_predictions, output_predictions, list_of_shp_files, factor_combs) {
  
  # for bulk_queue 
  # ID.exp, ID.run, grid.size, cell.fraction, train.fraction, pseudoAbs.value, weights, adm, pred_tag
  
  # ------------------------------------- Extract factors 
  
  exp_ID <- x$ID.exp
  cat("ID exp =", exp_ID, "\n")
  
  run_ID <- x$ID.run
  cat("ID run =", run_ID, "\n")
  
  grid.size <- x$grid.size
  cat("grid size =", grid.size, "\n")
  
  cell.fraction <- x$cell.fraction
  cat("cell fraction =", cell.fraction, "\n")
  
  train.fraction <- x$train.fraction
  cat("train fraction =", train.fraction, "\n")
  
  pseudoAbs.value <- x$pseudoAbs.value
  cat("pseudo absence value =", pseudoAbs.value, "\n")

  pseudoAbs.prop <- x$pseudoAbs.prop
  cat("pseudo absence proportion =", pseudoAbs.prop, "\n")
  
  weights <- x$weights
  cat("Weights =", weights, "\n")

  adm <- x$adm
  cat("admin unit of predictions =", adm, "\n")
  
  pred_tag <- as.character(x$pred_tag)
  cat("tag of predictors =", pred_tag, "\n")

  
  # ---------------------------------------- Index lists of input files
  
  
  # Get dataset for RF training/validating
  dataset <- list_of_data[[1]]
    
  # Get dataset for predictions
  admin_dataset <- list_of_data[[adm + 1]]
  
  # Get shapefiles 
  country_shp_file <- list_of_shp_files[[1]]
  
  adm_shp_file <- list_of_shp_files[[adm + 1]]
  
  
  # ---------------------------------------- Modify datasets 
  
  
  dataset[dataset$type == "pseudoAbsence", dependent_variable] <- pseudoAbs.value 
  
  # Weighting down pseudo absences
  if(weights == 1)  
  {
    dataset$new.weight <- 1
    dataset[dataset$type == "pseudoAbsence", "new.weight"] <- 0.25
  }
  
  # weighting down pseudo absences and x2 up high values
  if(weights == 2)
  {    
    dataset$new.weight <- 1
    dataset[dataset$type == "pseudoAbsence", "new.weight"] <- 0.25
    
    # assing higher weight to the 18 greatest values
    dataset[1:18, "new.weight"] <- 2
  }  
  
  # weighting down pseudo absences and x5 up high values
  if(weights == 3)
  {    
    dataset$new.weight <- 1
    dataset[dataset$type == "pseudoAbsence", "new.weight"] <- 0.25
    
    # assing higher weight to the 18 greatest values
    dataset[1:18, "new.weight"] <- 5
  }  

  # weighting down pseudo absences and x10 up high values
  if(weights == 4)
  {    
    dataset$new.weight <- 1
    dataset[dataset$type == "pseudoAbsence", "new.weight"] <- 0.25
    
    # assing higher weight to the 18 greatest values
    dataset[1:18, "new.weight"] <- 10
  }  

  # weighting based on 1/variance 
  if(weights == 5)
  {    
    dataset$new.weight <- 1 / dataset$variance
    dataset[dataset$type == "pseudoAbsence", "new.weight"] <- mean (dataset[dataset$type != "pseudoAbsence", "new.weight"]) * 0.25
  }  

  # Remove missing values 
  row.has.na <- apply(admin_dataset, 1, function(x){any(is.na(x))})
  admin_dataset <- admin_dataset[!row.has.na,]
  
  # Drop unused factor levels
  admin_dataset$country <- factor(admin_dataset$country)
  admin_dataset$country_code <- factor(admin_dataset$country_code)
  admin_dataset$adm1 <- factor(admin_dataset$adm1)
  
  pred_comb_index <- which(names(all_predictor_combs) == pred_tag)
  
  combination.of.predictors <- all_predictor_combs[[pred_comb_index]]
  
  Run <- spatial.cv.rf (dataset = dataset,
                        full_dataset = admin_dataset, 
                        grid_size = grid.size, 
                        combination_of_predictors = combination.of.predictors, 
                        no_fits = 500, 
                        no_trees = 500,
                        min_node_size = 20,
                        cell_fraction = cell.fraction, 
                        train_fraction = train.fraction,
                        dependent_variable = dependent_variable,
                        link_function = "linear",
                        prediction_cutoff = 0.0045)
  
  model_diagnostics <- Run[[1]]
  dengue_dataset_predictions <- Run[[2]]
  full_dataset_predictions <- Run[[3]]
  
  if (run_ID == 1) {
    
    # Create name for output experimental design 
    factor_combinations_file_name <- sprintf("factor_combinations_%s%s", 
                                             paste("exp", exp_ID, sep = "_"), ".csv")
    
    # Create output directory 
    dir.create(file.path("output", "dengue_dataset", output_folder, 
                         paste("exp", exp_ID, sep = "_")),
               FALSE, TRUE)
    
    # Write out the experimental design
    write.table(factor_combs, 
                file.path("output", "dengue_dataset", output_folder, 
                          paste("exp", exp_ID, sep = "_"),
                          factor_combinations_file_name), 
                row.names = FALSE, sep = ",")
    
  }

  if (output_predictions) {
    
    # Create folder for output tables
    dir.create(file.path("output", "dengue_dataset", output_folder, 
                         paste("exp", exp_ID, sep = "_")),
               FALSE, TRUE)
  
    prediction_df_name <- sprintf("predictions_%s_%s%s", 
                                paste("exp", exp_ID, sep = "_"), 
                                paste("run", run_ID, sep = "_"), ".rds")
  
    # Write out predictions
    saveRDS(full_dataset_predictions, 
            file.path("output", "dengue_dataset", output_folder, 
                      paste("exp", exp_ID, sep = "_"), 
                      prediction_df_name))
  
  }
  
  if (plot_predictions) {
    
    # Rotate prediction df from wide to long to allow faceting plotting
    dengue_dataset_predictions_long <- melt(dengue_dataset_predictions, id.vars = c("country_code", "adm1", "y.data"), 
                                            variable.name = "dataset")
    
    # Plot of predictions vs Observations of dengue dataset
    RF.preds.vs.obs.plot.stratif.no.labels (run_id = run_ID,
                                            exp_id = exp_ID,
                                            diagnostics = model_diagnostics, 
                                            predictions = dengue_dataset_predictions_long,
                                            output_folder = output_folder)
    
    # Attach predictions of full dataset to shp file
    adm_shp_file_with_preds <- merge(adm_shp_file, full_dataset_predictions, by = "OBJECTID", all.x = TRUE)
    
    # Fix NAs
    adm_shp_file_with_preds@data$mean_pred[is.na(adm_shp_file_with_preds@data$mean_pred)] <- 0
    adm_shp_file_with_preds@data$sd_pred[is.na(adm_shp_file_with_preds@data$sd_pred)] <- 0
    
    # Map mean predictions of full dataset
    map_predictions_admin_scale (run_id = run_ID,
                                 exp_id = exp_ID,
                                 adm_shp_file = adm_shp_file_with_preds,
                                 country_border_shp = country_shp_file,
                                 y_var = "mean_pred",
                                 map_title = dependent_variable,
                                 map_colours = matlab.like(400),
                                 output_folder = output_folder)

    # Map sd of mean predictions of full dataset
    map_predictions_admin_scale (run_id = run_ID,
                                 exp_id = exp_ID,
                                 adm_shp_file = adm_shp_file_with_preds,
                                 country_border_shp = country_shp_file,
                                 y_var = "sd_pred",
                                 map_title = paste("SD", dependent_variable, sep = " "),
                                 map_colours = rev(heat.colors(400)),
                                 output_folder = output_folder)
  }
  
  Run

}
