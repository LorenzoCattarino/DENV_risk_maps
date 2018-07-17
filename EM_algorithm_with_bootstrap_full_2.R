
library(weights) # for wtd.cor()
library(ggplot2)

source(file.path("R", "prepare_datasets", "calculate_wgt_corr.R"))
source(file.path("R", "prepare_datasets", "set_pseudo_abs_weights.R"))
       

# define parameters ----------------------------------------------------------- 


parameters <- list(
  dependent_variable = "FOI",
  pseudoAbs_value = -0.02,
  grid_size = 5,
  shape_1 = 0,
  shape_2 = 5,
  shape_3 = 1e6,
  all_wgt = 1,
  no_samples = 200,
  no_predictors = 26)   

no_sample <- parameters$no_samples

no_exp <- 20
  
dts_names <- c("pred_vs_obs_plot_averages_all_data", 
               "pred_vs_obs_plot_averages_no_psAb")

base_info <- c("data_id", "ID_0", "ID_1", "o_j", "dataset", "scale")

out_path <- file.path("figures",
                      "EM_algorithm",
                      "bootstrap_models",
                      "grid_size_5")


# define variables ------------------------------------------------------------


in_path <- file.path("output",
                     "EM_algorithm",
                     "bootstrap_models",
                     "grid_size_5",
                     paste0("FOI_boot_model_", seq_len(no_exp)),
                     "scatter_plots")


# load data -------------------------------------------------------------------


foi_dataset <- read.csv(file.path("output", 
                                  "foi", 
                                  "All_FOI_estimates_and_predictors.csv"),
                        stringsAsFactors = FALSE) 

predictor_rank <- read.csv(file.path("output", 
                                     "variable_selection",
                                     "stepwise",
                                     "predictor_rank.csv"), 
                           stringsAsFactors = FALSE)


# pre processing --------------------------------------------------------------


no_predictors <- parameters$no_predictors

no_predictors_left <- no_predictors - no_exp

x_labels <- predictor_rank$name[no_predictors:(no_predictors_left+1)] 

foi_dataset$new_weight <- parameters$all_wgt

pAbs_wgt <- get_sat_area_wgts(foi_dataset, parameters)

foi_dataset[foi_dataset$type == "pseudoAbsence", "new_weight"] <- pAbs_wgt


# processing ------------------------------------------------------------------


for (j in seq_along(dts_names)){
  
  dts_name <- dts_names[[j]]
    
  out_ls <- vector("list", no_exp)
  
  for (i in seq_len(no_exp)){
    
    dat <- read.csv(file.path(in_path[i], paste0(dts_name, ".csv")), header = TRUE)
    
    dat <- dat[, setdiff(names(dat), c("admin_sd", "cell_sd"))]
    
    dat_long <- reshape(dat, 
                        idvar = c("data_id", "ID_0", "ID_1", "o_j", "dataset"),
                        varying = list(5:6), 
                        v.names = "value",
                        timevar = "scale",
                        times = c("admin", "cell"),
                        direction = "long",
                        new.row.names = sequence(prod(length(5:6), nrow(dat))))
    
    new_name <- paste0("value_", i)
    
    names(dat_long)[names(dat_long) == "value"] <- new_name
    
    out_ls[[i]] <- dat_long[, new_name, drop = FALSE]
  
  }
  
  out_mat <- do.call("cbind", out_ls)
  
  out_mat <- cbind(dat_long[, base_info], out_mat)
  
  ret <- dplyr::left_join(out_mat, foi_dataset[, c("data_id", "new_weight")])
  
  out_mat_long <- reshape(ret, 
                          idvar = c("data_id", "ID_0", "ID_1", "o_j", "dataset", "scale", "new_weight"),
                          varying = 7:(ncol(ret)-1), 
                          sep = "_", 
                          direction = "long",
                          timevar = "experiment")
  
  out_mat_long$dataset <- factor(out_mat_long$dataset, levels = c("train", "test"))
  
  corr_coeff <- plyr::ddply(out_mat_long, c("dataset", "scale", "experiment"), calculate_wgt_cor, "o_j", "value")
  
  p <- ggplot(corr_coeff) +
    geom_point(aes(x = experiment, y = V1, colour = scale), size = 1) +
    facet_wrap(as.formula(~dataset), ncol = 1) +
    scale_y_continuous("Pearson correlation coefficient") +
    scale_x_continuous("Steps", limits = c(0, 20), breaks = seq(1, 20, 1), labels = x_labels) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 5))
  
  dir.create(out_path, FALSE, TRUE)
  
  png(filename = file.path(out_path, paste0(dts_name, ".png")), 
      width = 10, 
      height = 12, 
      units = "cm", 
      pointsize = 12,
      res = 200)
  
  print(p)
  
  dev.off()
  
}
