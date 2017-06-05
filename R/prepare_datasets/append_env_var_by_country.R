append_env_var_by_country <- function(x, a, b, j_flds){
  
  out <- x[[1]]
  
  names(out) <- b[[1]]
  
  for (i in seq(2, length(x))){
    
    #cat("iteration =", i, "\n")
    
    vars <- a[[i]]
    #cat("variables =", vars, "\n")
    
    var_names <- b[[i]] 
    
    data_set <- x[[i]][, c(j_flds, vars)]
    
    out  <- merge (out, data_set, by = j_flds, all.x = TRUE)
    
    names(out)[names(out) %in% vars] <- var_names
  }
  
  out
  
}
