exp_max_algorithm_boot <- function(
  i, out_model_name, out_pred_name, ...){
  
  a <- out_model_name[i]
  b <- out_pred_name[i]
  
  exp_max_algorithm(out_model_name = a, out_pred_name = b, ...)
  
}
