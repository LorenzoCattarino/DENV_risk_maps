average_boot_samples_dim2 <- function(dat, ...){
  out_names <- c("mean", "sd", "lCI", "uCI", "interv", "median")
  mean_val <- rowMeans(dat, ...)
  st_dev <- apply(dat, 1, FUN = sd, ...)
  percentiles <- apply(dat, 1, FUN = quantile, probs = c(0.025, 0.975), ...)
  percentiles <- t(percentiles)
  l_b <- percentiles[, 1]
  u_b <- percentiles[, 2]
  interv <- u_b - l_b
  median <- apply(dat, 1, median, ...) 
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

test_NA <- function (x) {
  anyNA(x[as.character(1:200)])
}