# Remove dataset records for which there is at least one NA predictor value

remove_NA_rows <- function(dataset, predictors){
  
  xx <- subset(dataset,, predictors)
  
  row.has.na <- apply(xx, 1, anyNA)
  
  xxx <- dataset[!row.has.na, , drop = FALSE]

  xxx
  
}
  
