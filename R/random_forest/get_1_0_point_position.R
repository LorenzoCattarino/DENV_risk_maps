get_training_point_positions <- function(no_data, training_dataset){
  
  training.set <- rep(0, no_data)
  
  # get the unique index of the data points in the training set
  model.training.set <- unique(training_dataset$id) 
  
  training.set[model.training.set] <- 1
  
  training.set
}

get_validating_point_positions <- function(no_data, training_dataset){
  
  training.set <- rep(0, no_data)
  
  # get the unique index of the data points in the training set
  model.training.set <- unique(training_dataset$id) 
  
  training.set[model.training.set] <- 1
  
  validating.set <- 1 - training.set
  
  validating.set
}
