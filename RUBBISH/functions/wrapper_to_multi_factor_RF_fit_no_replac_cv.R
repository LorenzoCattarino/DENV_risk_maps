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

spatial.cv.rf <- function (
  dataset, full_dataset, grid_size, combination_of_predictors, no_fits, no_trees, min_node_size, 
  cell_fraction, train_fraction, dependent_variable, link_function, prediction_cutoff) {
  
  no_data <- nrow(dataset) 
  
  training.sets <- matrix(0, nrow = no_data, ncol = no_fits)
  prediction.sets <- matrix(0, nrow = no_data, ncol = no_fits)
  
  y.col <- dependent_variable
  x.cols <- combination_of_predictors
  all.cols <- c(y.col, x.cols) 
  y.data <- dataset[, y.col]
  x.data <- dataset[, x.cols, drop = FALSE]
  
  list_leng <- 2
  
  if(!is.null(full_dataset))
  {
    no_data_full_dataset <- nrow(full_dataset)
    prediction.sets_full_dataset <- matrix(0, nrow = no_data_full_dataset, ncol = no_fits)  
    x.data_full_dataset <- full_dataset[, x.cols, drop = FALSE]
  }
  
  for(i in seq(no_fits))
  {
    # cat("fit = ", i, "\n")
    
    # draw random distance values 
    rd <- runif(n = 1, min = 0, max = grid_size)
    rd2 <- runif(n = 1, min = 0, max = grid_size)
    
    # add rd to lat.grid and long.grid variables 
    dataset$lat.grid <- floor((dataset$latitude - rd) / grid_size)
    dataset$long.grid <- floor((dataset$longitude - rd2) / grid_size)
    min.long <- min(dataset$long.grid)
    width.long <- max(dataset$long.grid) - min.long + 1
    min.lat <- min(dataset$lat.grid)
    
    dataset$cell <- (dataset$lat.grid - min.lat) * width.long + dataset$long.grid - min.long
    
    cell.unique <- as.data.frame(unique(dataset$cell))
    colnames(cell.unique)[1] <- "cell"
    cell.unique$pick <- floor(runif(n = nrow(cell.unique), min = 0, max = 1 / cell_fraction))
    cell.unique.N <- nrow(cell.unique)
    model.data <- merge(x = dataset, y = cell.unique, by = "cell", all.x = TRUE)
    model.data$pick <- ifelse(model.data$pick == 0, 1, 0)
    
    model.data.N <- nrow(model.data)
    train.N <- floor(train_fraction * sum(model.data$pick == 1))
    model.training.index <- seq(model.data.N)
    model.training.set <- sample(x = model.training.index, size = train.N, replace = FALSE, prob = model.data$pick)
    
    training.sets[model.training.set, i] <- 1
    
    data.train <- dataset[training.sets[,i] == 1, all.cols]
    my_weights <- dataset[training.sets[,i] == 1, "new.weight"]
    
    frmla <- as.formula(paste(dependent_variable, ".", sep = " ~ "))
    
    RFmodel <- ranger(frmla, data = data.train, 
                      num.trees = no_trees, 
                      min.node.size = min_node_size, 
                      case.weights = my_weights, 
                      write.forest = TRUE)
    
    prediction.sets[, i] <- predict(RFmodel, x.data)$predictions
    
    if(!is.null(full_dataset))
    {
      prediction.sets_full_dataset[, i] <- predict(RFmodel, x.data_full_dataset)$predictions
    }
  }
  
  valid.sets <- 1 - training.sets
  
  training.sets.n <- rowSums(training.sets)
  valid.sets.n <- rowSums(valid.sets)
  
  
  # ---------------------------------------- Remove pseudo absences 
  
  
  pseudo_abs_logical <- dataset$type == "pseudoAbsence"
  
  y.data <- y.data[!pseudo_abs_logical] 
  prediction.sets <- prediction.sets [!pseudo_abs_logical, ]
  training.sets <- training.sets[!pseudo_abs_logical, ]
  valid.sets <- valid.sets[!pseudo_abs_logical, ]
  training.sets.n <- training.sets.n[!pseudo_abs_logical]
  valid.sets.n <- valid.sets.n[!pseudo_abs_logical]
  
  my_weights <- dataset$new.weight[!pseudo_abs_logical]
  
  
  # ---------------------------------------- Transform the linear predictor and set threshold for zeros 
  
  
  if(link_function == "linear")
  {
    prediction.sets [prediction.sets < 0] <- 0
    prediction.sets_full_dataset [prediction.sets_full_dataset < 0] <- 0
  }
  
  if(link_function == "squared")
  {
    prediction.sets [prediction.sets < 0] <- 0
    prediction.sets_full_dataset [prediction.sets_full_dataset < 0] <- 0    
    
    y.data <- sqrt(y.data)
    prediction.sets <- sqrt(prediction.sets)
    prediction.sets_full_dataset <- sqrt(prediction.sets_full_dataset)
  }
  
  if(link_function == "log")
  {
    y.data <- exp(y.data)
    prediction.sets <- exp(prediction.sets)
    prediction.sets_full_dataset <- exp(prediction.sets_full_dataset)
    
    prediction.sets [prediction.sets < 0.002] <- 0
    prediction.sets_full_dataset [prediction.sets_full_dataset < 0.002] <- 0
  }
  
  if(link_function == "logit")
  {
    y.data <- inv.logit(y.data)
    prediction.sets <- inv.logit(prediction.sets)
    prediction.sets_full_dataset <- inv.logit(prediction.sets_full_dataset)
    
    prediction.sets [prediction.sets < 0.002] <- 0
    prediction.sets_full_dataset [prediction.sets_full_dataset < 0.002] <- 0
  }
  
  if(link_function == "inverse")
  {
    y.data <- 1 / y.data
    prediction.sets <- 1 / prediction.sets
    prediction.sets_full_dataset <- 1 / prediction.sets_full_dataset
    
    prediction.sets [prediction.sets < 0.0002] <- 0
    prediction.sets_full_dataset [prediction.sets_full_dataset < 0.0002] <- 0
  }
  
  ### Take the average of predictions across fits (by dataset)
  
  mean.prediction.train <- rowSums(prediction.sets * training.sets) / training.sets.n
  mean.prediction.valid <- rowSums(prediction.sets * valid.sets) / valid.sets.n
  
  if(!is.null(full_dataset))
  {
    mean.predictions_full_dataset <- rowMeans(prediction.sets_full_dataset)
    sd.predictions_full_dataset <- apply(prediction.sets_full_dataset, 1, FUN = sd)
    
    mean.predictions_full_dataset[mean.predictions_full_dataset < prediction_cutoff] <- 0
    
    # Get indices of zeros 
    zero_logical <- mean.predictions_full_dataset == 0
  }
  
  ### Calculate correlation coefficient and RMS error
  
  corr_coeff_train <- wtd.cors(y.data, mean.prediction.train, my_weights)
  corr_coeff_valid <- wtd.cors(y.data, mean.prediction.valid, my_weights)  
  
  rmse.train <- sqrt(weighted.mean((y.data - mean.prediction.train)^2, my_weights))
  rmse.valid <- sqrt(weighted.mean((y.data - mean.prediction.valid)^2, my_weights))
  
  ### Store outputs
  
  if(!is.null(full_dataset))
  {
    list_leng <- 3
  }
  
  output_list <- vector("list", length = list_leng)
  diagnostics <- c(corr_coeff_train = 0,
                   corr_coeff_valid = 0,
                   rmse.train = 0,
                   rmse.valid = 0)
  
  diagnostics["corr_coeff_train"] <- corr_coeff_train
  diagnostics["corr_coeff_valid"] <- corr_coeff_valid
  diagnostics["rmse.train"] <- rmse.train
  diagnostics["rmse.valid"] <- rmse.valid
  
  output_list [[1]] <- diagnostics
  output_list [[2]] <- data.frame(country_code = dataset$country_code[!pseudo_abs_logical], 
                                  adm1 = dataset$adm1[!pseudo_abs_logical], 
                                  y.data, 
                                  train_set = mean.prediction.train, 
                                  test_set = mean.prediction.valid)
  
  if(!is.null(full_dataset)) {
    numeric_columns <- grep("band", names(full_dataset ), value = TRUE)
    
    output_list [[3]] <- cbind(data.frame(OBJECTID = full_dataset$unique_ID[!zero_logical],
                                          country = full_dataset$country[!zero_logical],
                                          population = full_dataset$population[!zero_logical],
                                          mean_pred = mean.predictions_full_dataset[!zero_logical], 
                                          sd_pred = sd.predictions_full_dataset[!zero_logical]), 
                               full_dataset[!zero_logical, numeric_columns])
    
    # Drop unused factor levels
    output_list [[3]]$country <- factor(output_list [[3]]$country)
    
  }
  
  output_list
  
}
