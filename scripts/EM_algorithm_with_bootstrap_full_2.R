# For each bootstrap sample of the original dataset, it creates a scatter plot of:  
#
# 1) admin unit observation vs admin unit prediction 
# 2) admin unit observation vs population weighted average of the square predictions (within admin unit)
# 3) admin unit observation vs population weighted average of the 1 km pixel predictions (within admin unit)
#
# NOTE: 1, 2 and 3 are for train and test sets separately (total of 6 plots per bootstrap sample)

library(reshape2)
library(ggplot2)
library(plyr)
library(weights) # for wtd.cor()

source(file.path("R", "plotting", "plot_RF_preds_vs_obs_by_cv_dataset.R"))
source(file.path("R", "prepare_datasets", "set_pseudo_abs_weights.R"))
source(file.path("R", "prepare_datasets", "calculate_sd.R"))
source(file.path("R", "prepare_datasets", "calculate_wgt_corr.R"))
source(file.path("R", "utility_functions.R"))


# define parameters -----------------------------------------------------------  


parameters <- list(
  shape_1 = 0,
  shape_2 = 5,
  shape_3 = 1e6,
  all_wgt = 1,
  pseudoAbs_value = c(-0.02, 0.5),
  resample_grid_size = 20,
  foi_offset = 0.03,
  no_trees = 500,
  min_node_size = 20,
  no_samples = 200,
  EM_iter = 10) 

mes_vars <- c("admin", "cell")

tags <- c("all_data", "no_psAb")

data_types_vec <- list(c("serology", "caseReport", "pseudoAbsence"),
                       c("serology", "caseReport"))


# load data -------------------------------------------------------------------


bootstrap_experiments <- read.csv(file.path("output", 
                                            "EM_algorithm", 
                                            "bootstrap_models", 
                                            "boostrap_fit_experiments.csv"),
                                  stringsAsFactors = FALSE)

foi_dataset <- read.csv(file.path("output", 
                                  "foi", 
                                  "All_FOI_estimates_and_predictors.csv"),
                        stringsAsFactors = FALSE) 


# pre processing --------------------------------------------------------------


no_samples <- parameters$no_samples

no_datapoints <- nrow(foi_dataset)

no_pseudoAbs <- sum(foi_dataset$type == "pseudoAbsence") 

no_pnts_vec <- c(no_datapoints, no_datapoints - no_pseudoAbs) 

foi_dataset$new_weight <- parameters$all_wgt

pAbs_wgt <- get_sat_area_wgts(foi_dataset, parameters)

foi_dataset[foi_dataset$type == "pseudoAbsence", "new_weight"] <- pAbs_wgt

bootstrap_experiments_by <- dplyr::group_by(bootstrap_experiments, exp_id, var, gs, no_pred)

bootstrap_experiments_uni <- dplyr::summarise_at(bootstrap_experiments_by, "rep_id", min)

write_out_csv(bootstrap_experiments_uni, 
              file.path("output", "EM_algorithm", "bootstrap_models"), 
              "boostrap_fit_experiments_uni.csv")


# start ----------------------------------------------------------------------- 


for (k in seq_len(nrow(bootstrap_experiments_uni))){
  
  model_type <- paste0("model_", bootstrap_experiments_uni[k, "exp_id"])
  
  cat("model type =", model_type, "\n")
  
  var_to_fit <- bootstrap_experiments_uni[k, "var"]
  
  if(var_to_fit == "FOI") {
    
    psAbs_val <- parameters$pseudoAbs_value[1]
    
  } else {
    
    psAbs_val <- parameters$pseudoAbs_value[2]
    
  }
  
  in_path <- file.path("output",
                       "EM_algorithm",
                       "bootstrap_models",
                       model_type,
                       "data_admin_predictions") 
  
  out_fig_path <- file.path("figures",
                            "EM_algorithm",
                            "bootstrap_models",
                            model_type,
                            "scatter_plots",
                            "boot_samples")
  
  out_fig_path_av <- file.path("figures",
                               "EM_algorithm",
                               "bootstrap_models",
                               model_type,
                               "scatter_plots")
  
  out_table_path <- file.path("output",
                              "EM_algorithm",
                              "bootstrap_models",
                              model_type,
                              "scatter_plots")
  
  for (j in seq_along(tags)) {
    
    no_pnts <- no_pnts_vec[j]
    
    dt_typ <- data_types_vec[[j]]
    
    tag <- tags[j]
    
    
    #### create objects for matrix algebric operations
    
    
    all_adm_preds <- matrix(0, nrow = no_pnts, ncol = no_samples)
    all_sqr_preds <- matrix(0, nrow = no_pnts, ncol = no_samples)
    #all_pxl_preds <- matrix(0, nrow = no_pnts, ncol = no_samples)
    train_ids <- matrix(0, nrow = no_pnts, ncol = no_samples)
    test_ids <- matrix(0, nrow = no_pnts, ncol = no_samples)
    
    
    #### second loop
    
    
    for (i in seq_len(no_samples)) {
      
      dts_nm <- paste0("sample_", i, ".rds")
      
      dts_1 <- readRDS(file.path(in_path, dts_nm))
      
      if(var_to_fit == "FOI"){
        
        dts_1[, c("o_j", "admin", "square")][dts_1[, c("o_j", "admin", "square")] < 0] <- 0
        
      } else {
        
        dts_1[, c("o_j", "admin", "square")][dts_1[, c("o_j", "admin", "square")] < 1] <- psAbs_val
        
      }
      
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
      
      # # rotate df from wide to long to allow faceting
      # dts_mlt <- melt(
      #   dts, 
      #   id.vars = c("data_id", "ADM_0", "ADM_1", "o_j", "dataset"),
      #   measure.vars = mes_vars,
      #   variable.name = "scale")
      # 
      # fl_nm <- paste0("pred_vs_obs_plot_sample_", i, "_", tag, ".png")
      # 
      # RF_preds_vs_obs_plot_stratif(
      #   df = dts_mlt,
      #   x = "o_j",
      #   y = "value",
      #   facet_var = "scale",
      #   file_name = fl_nm,
      #   file_path = out_fig_path)
      
    }
    
    
    #### calculate the mean across fits of the predictions (adm, sqr and pxl) 
    #### by train and test dataset separately
    
    
    train_sets_n <- rowSums(train_ids)
    test_sets_n <- rowSums(test_ids)
    
    mean_adm_pred_train <- rowSums(all_adm_preds * train_ids) / train_sets_n
    mean_adm_pred_test <- rowSums(all_adm_preds * test_ids) / test_sets_n
    
    mean_sqr_pred_train <- rowSums(all_sqr_preds * train_ids) / train_sets_n
    mean_sqr_pred_test <- rowSums(all_sqr_preds * test_ids) / test_sets_n
    
    #mean_pxl_pred_train <- rowSums(all_pxl_preds * train_ids) / train_sets_n
    #mean_pxl_pred_test <- rowSums(all_pxl_preds * test_ids) / test_sets_n
    
    sd_mean_adm_pred_train <- vapply(seq_len(no_pnts), calculate_sd, 1, all_adm_preds, train_ids)
    sd_mean_adm_pred_test <- vapply(seq_len(no_pnts), calculate_sd, 1, all_adm_preds, test_ids)
    
    sd_mean_sqr_pred_train <- vapply(seq_len(no_pnts), calculate_sd, 1, all_sqr_preds, train_ids)
    sd_mean_sqr_pred_test <- vapply(seq_len(no_pnts), calculate_sd, 1, all_sqr_preds, test_ids)
    
    av_train_preds <- data.frame(dts[,c("data_id", "ID_0", "ID_1", "o_j")],
                                 admin = mean_adm_pred_train,
                                 cell = mean_sqr_pred_train,
                                 admin_sd = sd_mean_adm_pred_train,
                                 cell_sd = sd_mean_sqr_pred_train,
                                 #pixel = mean_pxl_pred_train,
                                 dataset = "train")
    
    av_test_preds <- data.frame(dts[,c("data_id", "ID_0", "ID_1", "o_j")],
                                admin = mean_adm_pred_test,
                                cell = mean_sqr_pred_test,
                                admin_sd = sd_mean_adm_pred_test,
                                cell_sd = sd_mean_sqr_pred_test,
                                #pixel = mean_pxl_pred_test,
                                dataset = "test")
    
    all_av_preds <- rbind(av_train_preds, av_test_preds)
    write_out_csv(all_av_preds, out_table_path, paste0("pred_vs_obs_plot_averages_", tag, ".csv"))
    
    all_av_preds_mlt <- melt(
      all_av_preds,
      id.vars = c("data_id", "ID_0", "ID_1", "o_j", "dataset"),
      measure.vars = mes_vars,
      variable.name = "scale")
    
    fl_nm_av <- paste0("pred_vs_obs_plot_averages_", tag, ".png")
    
    ret <- dplyr::left_join(all_av_preds_mlt, foi_dataset[, c("data_id", "new_weight")])
    
    RF_preds_vs_obs_plot_stratif(df = ret,
                                 x = "o_j",
                                 y = "value",
                                 facet_var = "scale",
                                 file_name = fl_nm_av,
                                 file_path = out_fig_path_av)
    
  }
  
}
