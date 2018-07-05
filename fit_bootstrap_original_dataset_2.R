
library(ggplot2)
library(plyr)
library(weights) # for wtd.cor()

source(file.path("R", "prepare_datasets", "set_pseudo_abs_weights.R"))
source(file.path("R", "prepare_datasets", "calculate_sd.R"))
source(file.path("R", "prepare_datasets", "calculate_wgt_corr.R"))
source(file.path("R", "utility_functions.R"))


# define parameters -----------------------------------------------------------  


parameters <- list(
  dependent_variable = "FOI",
  grid_size = 5,
  b = 0,
  c = 5,
  d = 1.6e6,
  pseudoAbs_value = -0.02,
  all_wgt = 1,
  wgt_limits = c(1, 500),
  no_samples = 200)   

mes_vars <- "admin"

tags <- c("all_data", "no_psAb")

data_types_vec <- list(c("serology", "caseReport", "pseudoAbsence"),
                       c("serology", "caseReport"))


# define variables ------------------------------------------------------------


var_to_fit <- parameters$dependent_variable

psAbs_val <- parameters$pseudoAbs_value

in_path <- file.path("output", "original_dataset_fits", "predictions_data") 

out_fig_path_av <- file.path("figures", "original_dataset_fits", "scatter_plots")

out_table_path <- file.path("output", "original_dataset_fits", "scatter_plots")


# load data -------------------------------------------------------------------


foi_dataset <- read.csv(file.path("output", 
                                  "foi", 
                                  "All_FOI_estimates_linear_env_var_area_salje.csv"),
                        stringsAsFactors = FALSE) 


# pre processing --------------------------------------------------------------


no_samples <- parameters$no_samples

no_datapoints <- nrow(foi_dataset)

no_pseudoAbs <- sum(foi_dataset$type == "pseudoAbsence") 

no_pnts_vec <- c(no_datapoints, no_datapoints - no_pseudoAbs) 

foi_dataset$new_weight <- parameters$all_wgt

pAbs_wgt <- get_sat_area_wgts(foi_dataset, parameters)

foi_dataset[foi_dataset$type == "pseudoAbsence", "new_weight"] <- pAbs_wgt


# start ----------------------------------------------------------------------- 


for (j in seq_along(tags)) {
  
  no_pnts <- no_pnts_vec[j]
  
  dt_typ <- data_types_vec[[j]]
  
  tag <- tags[j]
  
  
  #### create objects for matrix algebric operations
  
  
  all_adm_preds <- matrix(0, nrow = no_pnts, ncol = no_samples)
  train_ids <- matrix(0, nrow = no_pnts, ncol = no_samples)
  test_ids <- matrix(0, nrow = no_pnts, ncol = no_samples)
  
  
  #### second loop
  
  
  for (i in seq_len(no_samples)) {
    
    dts_nm <- paste0("predictions_", i, ".rds")
    
    dts_1 <- readRDS(file.path(in_path, dts_nm))
    
    dts_1[, c("FOI", "admin")][dts_1[, c("FOI", "admin")] < 0] <- 0

    dts <- dts_1[dts_1$type %in% dt_typ, ]
    
    
    #####
    
    all_adm_preds[,i] <- dts$admin
    train_ids[,i] <- dts$train
    test_ids[,i] <- 1 - dts$train
    
    #####
    
    
    names(dts)[names(dts) == "train"] <- "dataset"
    
    dts$dataset <- factor(x = dts$dataset, levels = c(1, 0), labels = c("train", "test"))
    
  }
  
  
  #### calculate the mean across fits of the predictions (adm, sqr and pxl) 
  #### by train and test dataset separately
  
  
  train_sets_n <- rowSums(train_ids)
  test_sets_n <- rowSums(test_ids)
  
  mean_adm_pred_train <- rowSums(all_adm_preds * train_ids) / train_sets_n
  mean_adm_pred_test <- rowSums(all_adm_preds * test_ids) / test_sets_n
  
  sd_mean_adm_pred_train <- vapply(seq_len(no_pnts), calculate_sd, 1, all_adm_preds, train_ids)
  sd_mean_adm_pred_test <- vapply(seq_len(no_pnts), calculate_sd, 1, all_adm_preds, test_ids)
  
  av_train_preds <- data.frame(dts[,c("data_id", "ID_0", "ID_1", "FOI")],
                               admin = mean_adm_pred_train,
                               admin_sd = sd_mean_adm_pred_train,
                               dataset = "train")
  
  av_test_preds <- data.frame(dts[,c("data_id", "ID_0", "ID_1", "FOI")],
                              admin = mean_adm_pred_test,
                              admin_sd = sd_mean_adm_pred_test,
                              dataset = "test")
  
  all_av_preds <- rbind(av_train_preds, av_test_preds)
  write_out_csv(all_av_preds, out_table_path, paste0("pred_vs_obs_plot_averages_", tag, ".csv"))
  
  fl_nm_av <- paste0("pred_vs_obs_plot_averages_", tag, ".png")
  
  ret <- dplyr::left_join(all_av_preds, foi_dataset[, c("data_id", "new_weight")])
  
  df <- ret
  x = "FOI"
  y = "admin"
  file_name <- fl_nm_av
  file_path <- out_fig_path_av
  
  x_values <- pretty(df[, x], n = 5)
  y_values <- pretty(df[, y], n = 5)
  min_x_value <- min(x_values)
  max_x_value <- max(x_values)
  min_y_value <- min(y_values)
  max_y_value <- max(y_values)
  
  corr_coeff <- ddply(df, "dataset", calculate_wgt_cor, x, y)
  
  facet_plot_names_x <- as_labeller(c(admin = "Level 1 administrative unit",
                                      cell = "20 km pixel"))
  
  facet_plot_names_y <- as_labeller(c(train = "Train set",
                                      test = "Test set"))
  
  p <- ggplot(df, aes_string(x = x, y = y)) +
    facet_grid(. ~ dataset,
               labeller = labeller(dataset = facet_plot_names_y)) + 
    geom_point(aes_string(x = x, y = y), size = 0.8) +
    geom_abline(slope = 1, intercept = 0, linetype = 2) +
    scale_x_continuous("Observations",  
                       limits = c(min_x_value, max_x_value), 
                       breaks = x_values,
                       labels = x_values) +
    scale_y_continuous("Predictions", 
                       #limits = c(min_y_value, max_y_value), 
                       breaks = y_values,
                       labels = y_values) +
    theme(axis.title.x = element_text(size = 12, margin = margin(t = 20)),
          axis.title.y = element_text(size = 12, margin = margin(r = 20)),
          axis.text.x = element_text(size = 10),
          axis.text.y = element_text(size = 10),
          plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
          strip.text.x = element_text(size = 12),
          strip.text.y = element_text(size = 12))
  
  p2 <- p +
    geom_text(data = corr_coeff, 
              aes(x = x_values[length(x_values)-1], 
                  y = min_y_value, 
                  hjust = 1, 
                  label = paste0("italic(r) == ", V1)),
              parse = TRUE,
              inherit.aes = FALSE,
              size = 4) +
    facet_grid(. ~ dataset,
               labeller = labeller(dataset = facet_plot_names_y))
  
  dir.create(file_path, FALSE, TRUE)
  
  png(filename = file.path(file_path, file_name), 
      width = 16, 
      height = 12, 
      units = "cm", 
      pointsize = 12,
      res = 200)
  
  print(p2)
  
  dev.off()
  
}
