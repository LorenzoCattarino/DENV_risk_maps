reset_no_transmission <- function(dts_1, parms){
  
  var_to_fit <- parms$dependent_variable
  
  psAbs_val <- parms$pseudoAbs_value[var_to_fit]
  
  if(var_to_fit == "FOI"){
    
    dts_1[, c("o_j", "admin", "mean_p_i")][dts_1[, c("o_j", "admin", "mean_p_i")] < 0] <- 0
    
  } else {
    
    dts_1[, c("o_j", "admin", "mean_p_i")][dts_1[, c("o_j", "admin", "mean_p_i")] < 1] <- psAbs_val
    
  }
  
  dts_1

}