# Creates a scatter plot of:  
#
# 1) admin unit observation vs admin unit prediction 
# 2) admin unit observation vs population weighted average of the square predictions (within admin unit)
# 3) admin unit observation vs population weighted average of the 1 km pixel predictions (within admin unit)


library(reshape2)
library(ggplot2)
library(plyr)
library(weights) # for wtd.cor()

source(file.path("R", "plotting", "plot_RF_preds_vs_obs_by_cv_dataset.R"))
source(file.path("R", "prepare_datasets", "set_pseudo_abs_weights.R"))
source(file.path("R", "prepare_datasets", "calculate_wgt_corr.R"))
source(file.path("R", "utility_functions.R"))
source(file.path("R", "create_parameter_list.R"))


# define parameters -----------------------------------------------------------  


extra_prms <- list(id = 13,
                   dependent_variable = "Z",
                   no_predictors = 26)

mes_vars <- c("admin", "square")

tags <- c("all_data", "no_psAb")

data_types_vec <- list(c("serology", "caseReport", "pseudoAbsence"),
                       c("serology", "caseReport"))

foi_dts_nm <- "All_FOI_estimates_and_predictors.csv"


# define variables ------------------------------------------------------------


parameters <- create_parameter_list(extra_params = extra_prms)

model_id <- parameters$id

var_to_fit <- parameters$dependent_variable

psAbs_val <- parameters$pseudoAbs_value

model_type <- paste0("model_", model_id)

if(var_to_fit == "FOI" | var_to_fit == "Z") {
  
  pseudoAbs_value <- parameters$pseudoAbs_value[1]
  
} else {
  
  pseudoAbs_value <- parameters$pseudoAbs_value[2]
  
}

in_path <- file.path("output",
                     "EM_algorithm",
                     "best_fit_models",
                     model_type,
                     "predictions_data") 

out_fig_path_av <- file.path("figures",
                             "EM_algorithm",
                             "best_fit_models",
                             model_type,
                             "scatter_plots")

out_table_path <- file.path("output",
                            "EM_algorithm",
                            "best_fit_models",
                            model_type,
                            "scatter_plots")


# load data ------------------------------------------------------------------- 


foi_data <- read.csv(file.path("output", "foi", foi_dts_nm),
                        stringsAsFactors = FALSE) 


# pre processing --------------------------------------------------------------


no_datapoints <- nrow(foi_data)

no_pseudoAbs <- sum(foi_data$type == "pseudoAbsence") 

no_pnts_vec <- c(no_datapoints, no_datapoints - no_pseudoAbs) 

foi_data$new_weight <- parameters$all_wgt

pAbs_wgt <- get_sat_area_wgts(foi_data, parameters)

foi_data[foi_data$type == "pseudoAbsence", "new_weight"] <- pAbs_wgt


# start -----------------------------------------------------------------------


for (j in seq_along(tags)) {
  
  no_pnts <- no_pnts_vec[j]
  
  dt_typ <- data_types_vec[[j]]
  
  tag <- tags[j]
  
  dts_nm <- paste0("all_scale_predictions.rds")
  
  dts_1 <- readRDS(file.path(in_path, dts_nm))
  
  if(var_to_fit == "FOI" | var_to_fit == "Z"){
    
    dts_1[, c("o_j", "admin", "square")][dts_1[, c("o_j", "admin", "square")] < 0] <- 0
    
  } else {
    
    dts_1[, c("o_j", "admin", "square")][dts_1[, c("o_j", "admin", "square")] < 1] <- pseudoAbs_value
    
  }
  
  dts <- dts_1[dts_1$type %in% dt_typ, ]
  
  all_av_preds_mlt <- melt(
    dts,
    id.vars = c("data_id", "ID_0", "ID_1", "o_j"),
    measure.vars = mes_vars,
    variable.name = "scale")
  
  ret <- dplyr::left_join(all_av_preds_mlt, foi_data[, c("data_id", "new_weight")])
  
  fl_nm_av <- paste0("pred_vs_obs_plot_averages_", tag, ".png")
  
  df <- ret
  x <- "o_j"
  y <- "value"
  facet_var <- "scale" 
  
  x_values <- pretty(df[, x], n = 5)
  y_values <- pretty(df[, y], n = 5)
  min_x_value <- min(x_values)
  max_x_value <- max(x_values)
  min_y_value <- min(y_values)
  max_y_value <- max(y_values)
  
  corr_coeff <- ddply(df, "scale", calculate_wgt_cor, "o_j", "value")
  
  facet_plot_names_x <- as_labeller(c(admin = "Level 1 administrative unit",
                                      square = "20 km pixel"))
  
  p <- ggplot(df, aes(x = "o_j", y = "value")) +
    facet_grid(. ~ scale,
               labeller = labeller(scale = facet_plot_names_x)) + 
    geom_point(aes_string(x = x, y = y), size = 1) +
    geom_abline(slope = 1, intercept = 0, linetype = 2) +
    scale_x_continuous("Observations",  
                       limits = c(min_x_value, max_x_value), 
                       breaks = x_values,
                       labels = x_values) +
    scale_y_continuous("Predictions", 
                       #limits = c(min_y_value, max_y_value), 
                       breaks = y_values,
                       labels = y_values) +
    theme(axis.title.x = element_text(size = 15, margin = margin(t = 20)),
          axis.title.y = element_text(size = 15, margin = margin(r = 20)),
          axis.text.x = element_text(size = 11),
          axis.text.y = element_text(size = 11),
          plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
          strip.text.x = element_text(size = 12),
          strip.text.y = element_text(size = 12))
  
  p2 <- p +
    geom_text(data = corr_coeff, 
              aes(x = x_values[length(x_values)-1], y = min_y_value, hjust = 1, label = paste0("italic(r) == ", V1)),
              parse = TRUE,
              inherit.aes = FALSE,
              size = 4) +
    facet_grid(. ~ scale,
               labeller = labeller(scale = facet_plot_names_x))
  
  dir.create(out_fig_path_av, FALSE, TRUE)
  
  png(filename = file.path(out_fig_path_av, fl_nm_av), 
      width = 16, 
      height = 8, 
      units = "cm", 
      pointsize = 12,
      res = 200)
  
  print(p2)
  
  dev.off()
  
}
