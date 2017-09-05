wrapper_to_mean_across_fits <- function(i, var_names, out_list){
  
  fl <- out_list[[i]] 
  
  ret <- mean_across_fits(fl)
  
  col_nms <- paste0(var_names[i], "_", colnames(ret))
  
  setNames(ret, col_nms)

}
