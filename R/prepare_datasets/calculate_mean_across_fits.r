mean_across_fits <- function(
  i, var_names, fl_pth) {
  
  out_names <- c("mean" , "l_CI", "u_CI")
  
  fl_nm <- paste0(var_names[i], ".rds")
  
  fl <- readRDS(file.path(fl_pth, fl_nm))
  
  # check if there is only one record in the dataset
  
  if(is.null(dim(fl))) {
    
    mean_val <- mean(fl)
    
    percentiles <- quantile(fl, probs = c(0.025, 0.975))
    
    l_b <- percentiles[1]
    u_b <- percentiles[2]
    
  } else {
    
    mean_val <- rowMeans(fl)
    
    percentiles <- apply(fl, 1, FUN = quantile, probs = c(0.025, 0.975))
    
    percentiles <- t(percentiles)
    
    l_b <- percentiles[, 1]
    u_b <- percentiles[, 2]
    
  }
  
  col_nms <- paste0(var_names[i], "_", out_names)
  
  setNames(data.frame(mean_val, l_b, u_b), col_nms)

}