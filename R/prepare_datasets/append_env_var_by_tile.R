append_env_var_by_tile <- function(x, a, b){
  
  out <- x[[1]]
  
  names(out) <- b[[1]]
  
  for (i in seq(2, length(x))){
    
    cat("dataset no =", i, "\n")
    
    dataset <- x[i]
    
    if(!is.na(dataset))
    {
      
      vars <- a[[i]]
      cat("variables =", vars, "\n")
      
      sub_dataset <- dataset[[1]][, vars]
      
      out <- cbind(out, sub_dataset)
    
      var_names <- b[[i]] 
    
      names(out)[names(out) %in% vars] <- var_names
      
    }
  
  }
  
  out
  
}
