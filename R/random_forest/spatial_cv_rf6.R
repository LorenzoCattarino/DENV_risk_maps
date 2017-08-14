spatial.cv.rf <- function(dataset, grid_size, combination_of_predictors, no_fits, no_trees, cell_fraction, train_fraction)
{
  mod.name.base <- paste("A85.Lnk.RF_priorsel3.wS", grid_size, sep="_")
  rmse.fits.train <- rep(0,no_fits)
  rmse.fits.valid <- rep(0,no_fits)
  corr.coeff.train <- rep(0,no_fits)
  corr.coeff.valid <- rep(0,no_fits
                          )  
  for(i in seq(no_fits))
  {
    rd <- runif(n=1, min=0, max=grid_size)
    rd2 <- runif(n=1, min=0, max=grid_size)   
    
    dataset$lat.grid <- floor((dataset$latitude-rd)/grid_size)
    dataset$long.grid <- floor((dataset$longitude-rd2)/grid_size)
    min.long <- min(dataset$long.grid)
    width.long <- max(dataset$long.grid)-min.long+1
    min.lat <- min(dataset$lat.grid)
    
    dataset$cell <- (dataset$lat.grid-min.lat)*width.long+dataset$long.grid-min.long

    cell.unique <- as.data.frame(unique(dataset$cell))
    colnames(cell.unique)[1] <- "cell"
    
    cell.unique$pick <- floor(runif(n = nrow(cell.unique), min = 0, max = 1/cell_fraction))
    model.data <- join(dataset,cell.unique)
    model.data$pick <- ifelse(model.data$pick==0,1,0)
    
    model.data$RF.weight <- 1

    model.data.N=nrow(model.data)
    model.data$RF.weight.spatial<-model.data$RF.weight*model.data$pick
    train.N=floor(train_fraction*sum(model.data$pick==1))
    model.training.index=seq(model.data.N)
    model.training.set<-sample(x = model.training.index, size = train.N, replace = FALSE, prob = model.data$RF.weight.spatial)
    
    model.data.train<-model.data[model.training.set,]
    model.data.valid<-model.data[-model.training.set,]
    
    x.cols <- combination_of_predictors 
    x.data <- model.data[,x.cols,drop = FALSE]
    
    y.col=2
    y.data<-model.data[,y.col]
    
    x.data.train<-model.data.train[,x.cols,drop = FALSE]
    x.data.valid<-model.data.valid[,x.cols,drop = FALSE]
    
    y.data.train<-model.data.train[,y.col]
    y.data.valid<-model.data.valid[,y.col]
    
    RFmodel <- randomForest(x = x.data.train, y = y.data.train, ntree = no_trees, importance = TRUE)

    y.data.valid.pred <- predict(RFmodel, x.data.valid)

    rmse.fits.train[i] <- sqrt(mean((y.data.train-y.data.train.pred)^2))
    rmse.fits.valid[i] <- sqrt(mean((y.data.valid-y.data.valid.pred)^2))
    corr.coeff.train[i] <- cor(y.data.train, y.data.train.pred)
    corr.coeff.valid[i] <- cor(y.data.valid, y.data.valid.pred)
  }
  
  corr.coeff.train <- mean(corr.coeff.train)
  corr.coeff.valid <- mean(corr.coeff.valid)
  rmse.train <- mean(rmse.fits.train)
  rmse.valid <- mean(rmse.fits.valid)

  output_list <- vector("list", length=1)
  diagnostics <- c(corr.coeff.train=0,
                   corr.coeff.valid=0,
                   rmse.train=0,
                   rmse.valid=0)
  
  diagnostics["corr.coeff.train"] <- corr.coeff.train
  diagnostics["corr.coeff.valid"] <- corr.coeff.valid
  diagnostics["rmse.train"] <- rmse.train
  diagnostics["rmse.valid"] <- rmse.valid

  output_list[[1]] <- diagnostics
  output_list
}
