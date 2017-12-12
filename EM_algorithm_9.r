# Creates a scatter plot of:  
#
# 1) admin unit observation vs admin unit prediction 
# 2) admin unit observation vs population weighted average of the square predictions (within admin unit)
# 3) admin unit observation vs population weighted average of the 1 km pixel predictions (within admin unit)


library(reshape2)
library(ggplot2)
library(plyr)

source(file.path("R", "plotting", "plot_RF_preds_vs_obs_by_cv_dataset.r"))
source(file.path("R", "utility_functions.r"))


# ---------------------------------------- define parameters 


model_type <- "best_model_20km_3c"

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

out_fig_path_av <- file.path(
  "figures",
  "EM_algorithm",
  model_type,
  "scatter_plots")

out_table_path <- file.path(
  "output",
  "EM_algorithm",
  model_type,
  "scatter_plots")


# ---------------------------------------- load data 


foi_dataset <- read.csv(
  file.path("output", "foi", "All_FOI_estimates_linear_env_var_area.csv"),
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
  
  dts_nm <- paste0("all_scale_predictions.rds")
  
  dts_1 <- readRDS(file.path(in_path, dts_nm))
  
  dts_1[, c("o_j", "admin", "square")][dts_1[, c("o_j", "admin", "square")] < 0] <- 0
  
  dts <- dts_1[dts_1$type %in% dt_typ, ]
  
  all_av_preds_mlt <- melt(
    dts,
    id.vars = c("data_id", "ADM_0", "ADM_1", "o_j"),
    measure.vars = mes_vars,
    variable.name = "scale")
  
  fl_nm_av <- paste0("pred_vs_obs_plot_averages_", tag, ".png")
  
  #browser()
  
  df <- all_av_preds_mlt
  x <- "o_j"
  y <- "value"
  facet_var <- "scale" 
  
  x_values <- pretty(df[, x], n = 5)
  y_values <- pretty(df[, y], n = 5)
  min_x_value <- min(x_values)
  max_x_value <- max(x_values)
  min_y_value <- min(y_values)
  max_y_value <- max(y_values)
  
  corr_coeff <- ddply(df, "scale", function(d.sub) round(cor(d.sub[,x], d.sub[,y]), 3))
  
  facet_plot_names_x <- as_labeller(c(admin = "Level 1 administrative unit",
                                      square = "20 km pixel"))
  
  p <- ggplot(df, aes(x = "o_j", y = "value")) +
    facet_grid(. ~ scale,
               labeller = labeller(scale = facet_plot_names_x)) + 
    geom_point(aes_string(x = x, y = y), size = 1) +
    geom_abline(slope = 1, intercept = 0, linetype = 2) +
    scale_x_continuous("Observations",  
                       #limits = c(min_x_value, max_x_value), 
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
              size = 5) +
    facet_grid(. ~ scale,
               labeller = labeller(scale = facet_plot_names_x))
  
  dir.create(out_fig_path_av, FALSE, TRUE)
  
  png(filename = file.path(out_fig_path_av, fl_nm_av), 
      width = 10, 
      height = 5, 
      units = "in", 
      pointsize = 12,
      res = 200)
  
  print(p2)
  
  dev.off()
  
}
