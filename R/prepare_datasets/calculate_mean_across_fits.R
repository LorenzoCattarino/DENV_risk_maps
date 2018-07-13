wrapper_to_average_bsamples <- function(j, 
                                        vars, 
                                        in_path, 
                                        out_path, 
                                        col_names){
  
  # browser()
  
  my_var <- vars[j]
  
  dat <- readRDS(file.path(in_path, paste0(my_var, ".rds")))
  
  ret <- average_boot_samples_dim2(dat[, col_names])
  
  base_info <- dat[, setdiff(names(dat), col_names)]
  
  ret2 <- cbind(base_info, ret)
  
  out_name <- paste0(my_var, "_mean.rds")
  
  write_out_rds(ret2, out_path, out_name)
  
  # loop(scenario_ids,
  #      calculate_mean_of_burden_predictions_for_different_scenarios,
  #      in_path = in_path, 
  #      my_var = my_var,
  #      col_names = col_names, 
  #      base_info = base_info, 
  #      out_path = out_path,
  #      parallel = TRUE)
  
}

average_boot_samples_dim2 <- function(dat){
  out_names <- c("mean", "sd", "lCI", "uCI", "interv", "median")
  mean_val <- rowMeans(dat)
  st_dev <- apply(dat, 1, FUN = sd)
  percentiles <- apply(dat, 1, FUN = quantile, probs = c(0.025, 0.975))
  percentiles <- t(percentiles)
  l_b <- percentiles[, 1]
  u_b <- percentiles[, 2]
  interv <- u_b - l_b
  median <- apply(dat, 1, median) 
  setNames(data.frame(mean_val, st_dev, l_b, u_b, interv, median), out_names)
}

average_boot_samples_dim1 <- function(dat){
  out_names <- c("mean", "sd", "lCI", "uCI", "interv", "median")
  mean_val <- mean(dat)
  st_dev <- sd(dat)
  percentiles <- quantile(dat, probs = c(0.025, 0.975))
  l_b <- percentiles[1]
  u_b <- percentiles[2]
  interv <- u_b - l_b
  median <- median(dat) 
  setNames(c(mean_val, st_dev, l_b, u_b, interv, median), out_names)
}

calculate_mean_of_burden_predictions_for_different_scenarios <- function(
  i, in_path, my_var,
  col_names, base_info, out_path){
  
  #browser()
  
  root_name <- paste0(my_var, "_all_squares")
  
  dat <- readRDS(file.path(in_path, sprintf("%s_%s%s", root_name, i, ".rds")))
  
  ret <- average_boot_samples_dim2(dat[, col_names])
  
  ret2 <- cbind(dat[, base_info], ret) 
  
  out_name <- sprintf("%s_%s_%s%s", my_var, "mean_all_squares", i, ".rds")
  
  write_out_rds(ret2, out_path, out_name)
  
}

get_grid_size_sd <- function(i, pred_ls){
  pred_ls[[i]]$sd
}

wrapper_to_mean_across_fits <- function(i, var_names, out_list){
  
  fl <- out_list[[i]] 
  
  ret <- mean_across_fits(fl)
  
  col_nms <- paste0(var_names[i], "_", colnames(ret))
  
  setNames(ret, col_nms)
  
}
