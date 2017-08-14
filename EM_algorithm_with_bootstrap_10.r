# For each bootstrap sample of the original dataset, it creates a scatter plot of:  
#
# 1) admin unit observation vs admin unit prediction 
# 2) admin unit observation vs population weighted average of the square predictions (within admin unit)
# 3) admin unit observation vs population weighted average of the 1 km pixel predictions (within admin unit)
#
# NOTE: 1, 2 and 3 are for train and test sets separately (total of 6 plots)

library(reshape2)
library(ggplot2)
library(plyr)

source(file.path("R", "random_forest", "RF_preds_vs_obs_stratified_plot.r"))
source(file.path("R", "random_forest", "get_lm_equation.r"))


# ---------------------------------------- define parameters 


model_type <- "boot_model_20km_cw"

no_fits <- 50

mes_vars <- c("admin", "square")

tags <- c("all_data", "no_psAb")

data_types_vec <- list(c("serology", "caseReport", "pseudoAbsence"),
                       c("serology", "caseReport"))

  
# ---------------------------------------- define variables


in_path <- file.path(
  "output",
  "EM_algorithm",
  model_type,
  "predictions_data") 

out_path <- file.path(
  "figures",
  "EM_algorithm",
  model_type,
  "scatter_plots",
  "boot_samples")
  
out_path_av <- file.path(
  "figures",
  "EM_algorithm",
  model_type,
  "scatter_plots")


# ---------------------------------------- load data 


foi_dataset <- read.csv(
  file.path("output", "foi", "All_FOI_estimates_linear_env_var.csv"),
  stringsAsFactors = FALSE) 


# ---------------------------------------- create some objects


no_datapoints <- nrow(foi_dataset)

no_pseudoAbs <- sum(foi_dataset$type == "pseudoAbsence") 

no_pnts_vec <- c(no_datapoints, no_datapoints - no_pseudoAbs) 
  

# ---------------------------------------- first loop 


for (j in seq_along(tags)) {

  no_pnts <- no_pnts_vec[j]
  
  dt_typ <- data_types_vec[[j]]
  
  tag <- tags[j]
  
  
  # ---------------------------------------- create objects for matrix algebric operations
  
  
  all_adm_preds <- matrix(0, nrow = no_pnts, ncol = no_fits)
  all_sqr_preds <- matrix(0, nrow = no_pnts, ncol = no_fits)
  #all_pxl_preds <- matrix(0, nrow = no_pnts, ncol = no_fits)
  train_ids <- matrix(0, nrow = no_pnts, ncol = no_fits)
  test_ids <- matrix(0, nrow = no_pnts, ncol = no_fits)
  
  
  # ---------------------------------------- second loop
  
  
  for (i in seq_len(no_fits)) {
    
    dts_nm <- paste0("all_scale_predictions_", i, ".rds")
    
    dts_1 <- readRDS(file.path(in_path, dts_nm))
    
    dts <- dts_1[dts_1$type %in% dt_typ, ]
    
    
    #####
    
    all_adm_preds[,i] <- dts$admin
    all_sqr_preds[,i] <- dts$square
    #all_pxl_preds[,i] <- dts$mean_pxl_pred
    train_ids[,i] <- dts$train
    test_ids[,i] <- 1 - dts$train
    
    #####
    
    
    names(dts)[names(dts) == "train"] <- "dataset"
    
    dts$dataset <- factor(x = dts$dataset, levels = c(1, 0), labels = c("train", "test"))
    
    # rotate df from wide to long to allow faceting
    dts_mlt <- melt(
      dts, 
      id.vars = c("data_id", "ADM_0", "ADM_1", "o_j", "dataset"),
      measure.vars = mes_vars,
      variable.name = "scale")
    
    fl_nm <- paste0("pred_vs_obs_plot_sample_", i, "_", tag, ".png")
    
    RF_preds_vs_obs_plot_stratif(
      df = dts_mlt,
      x = "o_j",
      y = "value",
      facet_var = "scale",
      file_name = fl_nm,
      file_path = out_path)
    
  }
  
  
  # ---------------------------------------- calculate the mean across fits of the predictions (adm, sqr and pxl) 
  # ---------------------------------------- by train and test dataset separately
  
  
  train_sets_n <- rowSums(train_ids)
  test_sets_n <- rowSums(test_ids)
  
  mean_adm_pred_train <- rowSums(all_adm_preds * train_ids) / train_sets_n
  mean_adm_pred_test <- rowSums(all_adm_preds * test_ids) / test_sets_n
  
  mean_sqr_pred_train <- rowSums(all_sqr_preds * train_ids) / train_sets_n
  mean_sqr_pred_test <- rowSums(all_sqr_preds * test_ids) / test_sets_n
  
  #mean_pxl_pred_train <- rowSums(all_pxl_preds * train_ids) / train_sets_n
  #mean_pxl_pred_test <- rowSums(all_pxl_preds * test_ids) / test_sets_n
  
  av_train_preds <- data.frame(dts[,c("data_id", "ADM_0", "ADM_1", "o_j")],
                               admin = mean_adm_pred_train,
                               square = mean_sqr_pred_train,
                               #pixel = mean_pxl_pred_train,
                               dataset = "train")
  
  av_test_preds <- data.frame(dts[,c("data_id", "ADM_0", "ADM_1", "o_j")],
                              admin = mean_adm_pred_test,
                              square = mean_sqr_pred_test,
                              #pixel = mean_pxl_pred_test,
                              dataset = "test")
  
  all_av_preds <- rbind(av_train_preds, av_test_preds)
  write.csv(all_av_preds, 
            file.path(out_path, 
                      paste0("pred_vs_obs_plot_averages_", tag, ".csv")), 
            row.names = FALSE)
  
  all_av_preds_mlt <- melt(
    all_av_preds, 
    id.vars = c("data_id", "ADM_0", "ADM_1", "o_j", "dataset"),
    measure.vars = mes_vars,
    variable.name = "scale")
  
  fl_nm_av <- paste0("pred_vs_obs_plot_averages_", tag, ".png")
  
  RF_preds_vs_obs_plot_stratif(
    df = all_av_preds_mlt,
    x = "o_j",
    y = "value",
    facet_var = "scale",
    file_name = fl_nm_av,
    file_path = out_path_av)
  
  # percentiles_train <- t(apply(produc_train, 1, quantile, probs = c(0.025, 0.975)))
  # percentiles_test <- t(apply(produc_test, 1, quantile, probs = c(0.025, 0.975)))
  # colnames(percentiles_train) <- c("low_perc_train", "up_perc_train")
  # colnames(percentiles_test) <- c("low_perc_test", "up_perc_test")

}
