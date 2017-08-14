calc_SSE <- function(
  y.data, valid.set, predictions, my_weights = NULL){

  y.data <- y.data * valid.set
  predictions <- predictions * valid.set
  
  if (!is.null(my_weights)) {
    
    my_weights <- my_weights * valid.set
    sq_error <- (my_weights * (y.data - predictions)^2) / sum(my_weights) 
  
  } else {
    
    sq_error <- (y.data - predictions)^2
  
  }
  
  sum(sq_error)

}
