
library(weights) # for wtd.cor()

source(file.path("R", "prepare_datasets", "set_pseudo_abs_weights.R"))
source(file.path("R", "prepare_datasets", "calculate_wgt_corr.R"))
source(file.path("R", "utility_functions.R"))


# define parameters -----------------------------------------------------------


parameters <- list(
  shape_1 = 0,
  shape_2 = 5,
  shape_3 = 1.6e6,
  pseudoAbs_value = c(-0.02, 0.5),
  all_wgt = 1)   

data_types_vec <- list(c("serology", "caseReport", "pseudoAbsence"),
                       c("serology", "caseReport"))

source_dts_names <- c("pred_vs_obs_plot_averages_all_data.csv", 
                      "pred_vs_obs_plot_averages_no_psAb.csv")

out_names <- c("correlation_coefficients_table_SI_all_data.csv",
               "correlation_coefficients_table_SI_no_psAb.csv")

idx <- 2


# define variables ------------------------------------------------------------


dt_typ <- data_types_vec[[idx]]

source_dts_name <- source_dts_names[idx]

out_name <- out_names[idx]


# load data -------------------------------------------------------------------


foi_data <- read.csv(file.path("output", "foi", "All_FOI_estimates_and_predictors.csv"),
                        stringsAsFactors = FALSE) 

best_fit_exp <- read.csv(file.path("output", 
                                   "EM_algorithm",
                                   "best_fit_models", 
                                   "best_fit_experiments.csv"),
                         stringsAsFactors = FALSE)

boot_fit_exp <- read.csv(file.path("output", 
                                   "EM_algorithm",
                                   "bootstrap_models", 
                                   "boostrap_fit_experiments_uni.csv"),
                         stringsAsFactors = FALSE)


# pre processing --------------------------------------------------------------


foi_data$new_weight <- parameters$all_wgt

pAbs_wgt <- get_sat_area_wgts(foi_data, parameters)

foi_data[foi_data$type == "pseudoAbsence", "new_weight"] <- pAbs_wgt

best_fit_exp_sub <- subset(best_fit_exp, no_pred != 23)

boot_fit_exp_sub <- subset(boot_fit_exp, no_pred != 23 & (gs == 5 | gs == 0.008333333))


# for best fit models ---------------------------------------------------------


best_fit_exp_sub$best_fit <- 0

for (i in seq_len(nrow(best_fit_exp_sub))){
  
  id <- best_fit_exp_sub[i, "id"]
  
  var_to_fit <- best_fit_exp_sub[i, "var"]
    
  no_pred <- best_fit_exp[i, "no_pred"]
    
  in_path <- file.path("output", 
                       "EM_algorithm",
                       "best_fit_models",
                       paste0("model_", id),
                       "predictions_data")

  dts <- readRDS(file.path(in_path, "all_scale_predictions.rds"))  
  
  dts_1 <- dplyr::rename(dts, cell = square)
  
  if(var_to_fit == "FOI"){
    
    psAbs_val <- parameters$pseudoAbs_value[1]
    
    dts_1[, c("o_j", "admin", "cell")][dts_1[, c("o_j", "admin", "cell")] < 0] <- 0
    
  } else {
    
    psAbs_val <- parameters$pseudoAbs_value[2]
    
    dts_1[, c("o_j", "admin", "cell")][dts_1[, c("o_j", "admin", "cell")] < 1] <- psAbs_val
    
  }
  
  dts_1 <- dts_1[dts_1$type %in% dt_typ, ]
  
  dts_1 <- dts_1[, setdiff(names(dts_1), "cell")]
    
  ret <- dplyr::left_join(dts_1, foi_data[, c("data_id", "new_weight")])
  
  best_fit_exp_sub[i, "best_fit"] <- calculate_wgt_cor(ret, "o_j", "admin")

}


# for bootstrap models --------------------------------------------------------


boot_fit_exp_sub$train <- 0
boot_fit_exp_sub$test <- 0

for (i in seq_len(nrow(boot_fit_exp_sub))){
  
  id <- boot_fit_exp_sub[i, "exp_id"]
  
  var_to_fit <- boot_fit_exp_sub[i, "var"]
  
  no_pred <- boot_fit_exp_sub[i, "no_pred"]
  
  in_path <- file.path("output", 
                       "EM_algorithm",
                       "bootstrap_models",
                       paste0("model_", id),
                       "scatter_plots")
  
  dts <- read.csv(file.path(in_path, source_dts_name))  
  
  dts_1 <- dts
  
  if(var_to_fit == "FOI"){
    
    psAbs_val <- parameters$pseudoAbs_value[1]
    
    dts_1[, c("o_j", "admin", "cell")][dts_1[, c("o_j", "admin", "cell")] < 0] <- 0
    
  } else {
    
    psAbs_val <- parameters$pseudoAbs_value[2]
    
    dts_1[, c("o_j", "admin", "cell")][dts_1[, c("o_j", "admin", "cell")] < 1] <- psAbs_val
    
  }
  
  dts_1 <- dts_1[, setdiff(names(dts_1), "cell")]
  
  ret <- dplyr::left_join(dts_1, foi_data[, c("data_id", "new_weight")])
  
  corr_coeff <- plyr::ddply(ret, "dataset", calculate_wgt_cor, "o_j", "admin")
  
  boot_fit_exp_sub[i, "test"] <- corr_coeff[corr_coeff$dataset == "test", "V1"]
  boot_fit_exp_sub[i, "train"] <- corr_coeff[corr_coeff$dataset == "train", "V1"]
  
}

boot_fit_exp_sub <- boot_fit_exp_sub[setdiff(names(boot_fit_exp_sub), c("exp_id", "rep_id"))]

final_out <- plyr::rbind.fill(best_fit_exp_sub, boot_fit_exp_sub)

final_out <- final_out[, c("id", "var", "no_pred", "gs", "best_fit", "train", "test")]

final_out$id <- seq_len(nrow(final_out))

# save
write_out_csv(final_out, file.path("output", "EM_algorithm"), out_name)
