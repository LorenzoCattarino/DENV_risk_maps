spatial.cv.rf <- function(dataset, grid_size, combination_of_predictors, no_fits, no_trees, cell_fraction, train_fraction)
{
  mod.name.base <- "A85.Lnk.RNG_spatial_prob50B.wSA"
  
  corr.fits.train <- rep (0,no_fits)
  corr.fits.valid <- rep (0,no_fits)
  
  rms.fits.train <- rep (0,no_fits)
  rms.fits.valid <- rep (0,no_fits)
  
  ranger.model.obj <- vector("list", length = no_fits)
  
  variable.importance <- matrix(0, nrow = length(combination_of_predictors), ncol = no_fits)
    
  for(i in seq(no_fits))
  {
    # draw random distance values 
    rd <- runif(n=1, min=0, max=grid_size)
    rd2 <- runif(n=1, min=0, max=grid_size)
   
    # add rd to lat.grid and long.grid variables 
    dataset$lat.grid <- floor((dataset$latitude-rd)/grid_size)
    dataset$long.grid <- floor((dataset$longitude-rd2)/grid_size)
    min.long <- min(dataset$long.grid)
    width.long <- max(dataset$long.grid)-min.long+1
    min.lat <- min(dataset$lat.grid)
  
    dataset$cell <- (dataset$lat.grid-min.lat)*width.long+dataset$long.grid-min.long
  
    cell.unique <- as.data.frame(unique(dataset$cell))
    colnames(cell.unique)[1] <- "cell"
    cell.unique$pick <- floor(runif(n = nrow(cell.unique), min = 0, max = 1/cell_fraction))
    cell.unique.N <- nrow(cell.unique)
    model.data <- merge(x = dataset, y = cell.unique, by = "cell", all.x = TRUE)
    #model.data <- join(dataset,cell.unique)
    model.data$pick <- ifelse(model.data$pick==0,1,0)
    cell.count <- as.data.frame(table(model.data$cell))
    colnames(cell.count)[1] <- "cell"
    colnames(cell.count)[2] <- "cell.count"
    model.data <- merge(x = model.data, y = cell.count, by = "cell", all.x = TRUE)
    #model.data <- join(model.data,cell.count,by = "cell",type="left")
    model.data <- model.data[,c(2:ncol(model.data),1)]
    
    #WeightsA<-model.data$Weight*model.data$Area_85/(model.data$Area_85+999)/sqrt(model.data$count_id_85)
    #model.data$RF.weight<-1/model.data$cell.count
    #model.data$RF.weight<-model.data$Weight*model.data$WeightA_85/sqrt(model.data$count_id_85)
  
    model.data$RF.weight <- model.data$new.weight
  
    model.data.N <- nrow(model.data)
    train.N <- floor(train_fraction*sum(model.data$pick==1))
    model.training.index <- seq(model.data.N)
    model.training.set <- sample(model.training.index,train.N,replace=FALSE,prob=model.data$pick)
  
    model.data.train <- model.data[model.training.set,]
    model.data.valid <- model.data[-model.training.set,]
    
    x.cols <- combination_of_predictors
    y.col <- which(names(dataset)=="FOI")
    all.cols <- c(y.col,x.cols)

    x.data <- model.data[,all.cols]
    x.data.train <- model.data.train[,all.cols]
    x.data.valid <- model.data.valid[,all.cols]
    
    y.data <- model.data[,y.col]
    y.data.train <- model.data.train[,y.col]
    y.data.valid <- model.data.valid[,y.col]
  
    RFmodel <- ranger(FOI ~ ., data = x.data.train, num.trees = no_trees, case.weights = model.data.train$RF.weight, 
                      write.forest = TRUE, importance = "permutation")
  
    y.data.train.pred <- RFmodel$predictions
    y.data.valid.pred <- predict(RFmodel,x.data.valid)$predictions
    
    # calculate outputs for each fit
    corr.fits.train[i] <- wtd.cors(y.data.train, y.data.train.pred, model.data.train$RF.weight)
    corr.fits.valid[i] <- wtd.cors(y.data.valid, y.data.valid.pred, model.data.valid$RF.weight)
    
    rms.fits.train[i] <- sqrt(weighted.mean((y.data.train-y.data.train.pred)^2,model.data.train$RF.weight))
    rms.fits.valid[i] <- sqrt(weighted.mean((y.data.valid-y.data.valid.pred)^2,model.data.valid$RF.weight))
    
    ranger.model.obj[[i]] <- RFmodel
    variable.importance[,i] <- RFmodel$variable.importance
  }
  
  y.data.pred <- sapply(ranger.model.obj, function(x) { predict(x, x.data)$predictions })
                        
  # calculate outputs across fits
  mean.corr.fits.train <- mean(corr.fits.train)
  mean.corr.fits.valid <- mean(corr.fits.valid)
  mean.rms.fits.train <- mean(rms.fits.train)
  mean.rms.fits.valid <- mean(rms.fits.valid)
  mean.y.data.pred <- rowMeans(y.data.pred)
  mean.variable.importance <- rowMeans(variable.importance)
  names(mean.variable.importance) <- names(RFmodel$variable.importance)
    
  output_list <- vector("list", length=3)
    
  output <- c(mean_corr_fits_train=0, 
              mean_corr_fits_valid=0, 
              mean_rms_fits_train=0, 
              mean_rms_fits_valid=0)
  
  # store outputs 
  output["mean_corr_fits_train"] <- mean.corr.fits.train
  output["mean_corr_fits_valid"] <- mean.corr.fits.valid
  output["mean_rms_fits_train"] <- mean.rms.fits.train
  output["mean_rms_fits_valid"] <- mean.rms.fits.valid
   
  output_list [[1]] <- output
  # output_list [[2]] <- data.frame(y.data.train = y.data.train, 
  #                                 y.data.train.pred = y.data.train.pred)
  # output_list [[3]] <- data.frame(y.data.valid = y.data.valid,
  #                                 y.data.valid.pred = y.data.valid.pred)
  output_list [[2]] <- data.frame(y.data = y.data,
                                  y.data.pred = mean.y.data.pred)
  output_list [[3]] <- mean.variable.importance
  
  output_list
}
