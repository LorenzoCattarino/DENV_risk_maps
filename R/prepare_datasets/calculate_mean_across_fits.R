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

get_grid_size_sd <- function(i, pred_ls){
  pred_ls[[i]]$sd
}

wrapper_to_mean_across_fits <- function(i, var_names, out_list){
  
  fl <- out_list[[i]] 
  
  ret <- mean_across_fits(fl)
  
  col_nms <- paste0(var_names[i], "_", colnames(ret))
  
  setNames(ret, col_nms)
  
}

combine_predictions_and_average <- function(x, parms, all_sqr_covariates){
  
  base_info <- c("cell", "latitude", "longitude", "population", "ID_0", "ID_1", "ID_2") 
  
  model_type <- paste0("model_", x$exp_id)
  
  cat("model type =", model_type, "\n")
  
  var_to_fit <- x$var
  
  foi_offset <- parms$foi_offset
  
  col_names <- as.character(seq_len(parms$no_samples))
  
  global_predictions_in_path <- file.path("output", 
                                          "predictions_world",
                                          "bootstrap_models",
                                          model_type,
                                          "boot_samples")
  
  out_path <- file.path("output", 
                      "predictions_world", 
                      "bootstrap_models",
                      model_type)
  
  fi <- list.files(global_predictions_in_path, 
                   pattern = "^sample",
                   full.names = TRUE)
  
  all_samples <- loop(fi, readRDS, parallel = FALSE)
  
  sqr_preds <- do.call("cbind", all_samples)
  
  if(var_to_fit =="FOI"){
    
    sqr_preds <- sqr_preds - foi_offset
    
  }
  
  sqr_preds[sqr_preds < 0] <- 0
  
  sqr_preds <- cbind(all_sqr_covariates[, base_info], sqr_preds)
  
  write_out_rds(sqr_preds, out_path, "response.rds")  
  
  ret <- average_boot_samples_dim2(sqr_preds[, col_names])
  
  ids_info <- sqr_preds[, setdiff(names(sqr_preds), col_names)]
  
  ret2 <- cbind(ids_info, ret)
  
  write_out_rds(ret2, out_path, "response_mean.rds")
  
}
