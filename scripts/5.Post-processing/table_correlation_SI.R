
library(weights) # for wtd.cor()

source(file.path("R", "utility_functions.R"))
source(file.path("R", "create_parameter_list.R"))
source(file.path("R", "prepare_datasets", "set_pseudo_abs_weights.R"))
source(file.path("R", "prepare_datasets", "calculate_wgt_corr.R"))


# define parameters -----------------------------------------------------------


data_types_vec <- list(c("serology", "caseReport", "pseudoAbsence"),
                       c("serology", "caseReport"))

source_dts_names <- c("pred_vs_obs_plot_averages_all_data.csv", 
                      "pred_vs_obs_plot_averages_no_psAb.csv")

out_names <- c("correlation_coefficients_table_SI_all_data.csv",
               "correlation_coefficients_table_SI_no_psAb.csv")


# define variables ------------------------------------------------------------


parameters <- create_parameter_list()


# load data -------------------------------------------------------------------


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

# will only report fit accuracy for random and 5km sampling
boot_fit_exp_sub <- subset(boot_fit_exp, !(gs %in% c(0.5, 1, 2, 10)))


# start -----------------------------------------------------------------------


for (j in seq_along(data_types_vec)){
  
  dt_typ <- data_types_vec[[j]]
  
  source_dts_name <- source_dts_names[j]
  
  out_name <- out_names[j]
  
  
  # for best fit models -------------------------------------------------------

  
  best_fit_exp$best_fit <- 0
  
  for (i in seq_len(nrow(best_fit_exp))){
    
    id <- best_fit_exp[i, "exp_id"]
    
    var_to_fit <- best_fit_exp[i, "var"]
    
    no_pred <- best_fit_exp[i, "no_pred"]
    
    in_path <- file.path("output", 
                         "EM_algorithm",
                         "best_fit_models",
                         paste0("model_", id),
                         "data_admin_predictions")
    
    dts <- readRDS(file.path(in_path, "all_scale_predictions.rds"))  
    
    # dts_1 <- dplyr::rename(dts, cell = square)
    
    psAbs_val <- parameters$pseudoAbs_value[var_to_fit]
    
    if(var_to_fit == "FOI"){
      
      dts[, c("o_j", "admin", "mean_p_i")][dts[, c("o_j", "admin", "mean_p_i")] < 0] <- 0
      
    } else {
      
      dts[, c("o_j", "admin", "mean_p_i")][dts[, c("o_j", "admin", "mean_p_i")] < 1] <- psAbs_val
      
    }
    
    dts_1 <- dts[dts$type %in% dt_typ, ]
    
    dts_1 <- dts_1[, setdiff(names(dts_1), "mean_p_i")]
    
    best_fit_exp[i, "best_fit"] <- calculate_wgt_cor(dts_1, "o_j", "admin")
    
  }
  
  
  # for bootstrap models ------------------------------------------------------
  
  
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
    
    psAbs_val <- parameters$pseudoAbs_value[var_to_fit]
    
    if(var_to_fit == "FOI"){
      
      dts[, c("o_j", "admin", "mean_p_i")][dts[, c("o_j", "admin", "mean_p_i")] < 0] <- 0
      
    } else {
      
      dts[, c("o_j", "admin", "mean_p_i")][dts[, c("o_j", "admin", "mean_p_i")] < 1] <- psAbs_val
      
    }
    
    dts_1 <- dts[, setdiff(names(dts), "mean_p_i")]
    
    corr_coeff <- plyr::ddply(dts_1, "dataset", calculate_wgt_cor, "o_j", "admin")
    
    boot_fit_exp_sub[i, "test"] <- corr_coeff[corr_coeff$dataset == "test", "V1"]
    boot_fit_exp_sub[i, "train"] <- corr_coeff[corr_coeff$dataset == "train", "V1"]
    
  }
  
  final_out <- plyr::rbind.fill(best_fit_exp, boot_fit_exp_sub)
  
  final_out <- dplyr::rename(final_out, id = exp_id)
  
  final_out <- final_out[, c("id", "var", "no_pred", "gs", "best_fit", "train", "test")]
  
  final_out$id <- seq_len(nrow(final_out))
  
  # save
  write_out_csv(final_out, file.path("output", "EM_algorithm"), out_name)
  
}
