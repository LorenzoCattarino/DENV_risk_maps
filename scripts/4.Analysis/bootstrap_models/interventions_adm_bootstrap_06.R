# Plot proportional reduction in infections, cases and hospitalized cases 
# for vaccination when optimally picking screening age

library(dplyr)
library(ggplot2)

out_fig_path <- file.path("figures", 
                          "predictions_world", 
                          "bootstrap_models",
                          "adm_1")

intervention_name <- "vaccine"

burden_measures <- c("infections", "cases", "hosp") 

y_axis_titles <- c("Reduction in infections", "Reduction in cases", "Reduction in hopsitalized cases")


# plotting ------------------------------------------------------------------


for (j in seq_along(burden_measures)) {
  
  my_var_name <- burden_measures[j]
  
  y_axis_title <- y_axis_titles[j]
  
  summary_table_orig_mean <- read.csv(file.path("output", 
                                                "predictions_world", 
                                                "bootstrap_models",
                                                "adm_1",
                                                paste0("prop_change_", my_var_name, "_mean_", intervention_name, ".csv")),
                                      header = TRUE)
  
  summary_table_orig_L95 <- read.csv(file.path("output", 
                                               "predictions_world", 
                                               "bootstrap_models",
                                               "adm_1",
                                               paste0("prop_change_", my_var_name, "_L95_", intervention_name, ".csv")),
                                     header = TRUE)
  
  summary_table_orig_U95 <- read.csv(file.path("output", 
                                               "predictions_world", 
                                               "bootstrap_models",
                                               "adm_1",
                                               paste0("prop_change_", my_var_name, "_U95_", intervention_name, ".csv")),
                                     header = TRUE)
  
  summary_table_orig <- summary_table_orig_mean
  
  summary_table_orig$lCI <- summary_table_orig_L95$mean - (1.92 * summary_table_orig$sd)
  
  summary_table_orig$uCI <- summary_table_orig_U95$mean + (1.92 * summary_table_orig$sd)
  
  summary_table <- summary_table_orig
  
  y_values <- seq(0, 0.5, 0.1)

  p <- ggplot(summary_table, aes(x = burden_measure, y = mean, ymin = lCI, ymax = uCI)) +
    geom_bar(stat = "identity", position = "dodge", width = 0.5, fill = "lightskyblue3") +
    geom_errorbar(width = .1, position = position_dodge(.9)) +
    facet_grid(. ~ phi_set_id) +
    xlab(NULL) +
    scale_y_continuous(y_axis_title,
                       breaks = y_values,
                       labels = paste0(y_values * 100, "%"),
                       limits = c(min(y_values), max(y_values)),
                       expand = expand_scale(mult = c(0, .05))) +
    theme_bw() +
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.y = element_text(size = 12),
          plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
          strip.text.x = element_text(size = 8))
  
  dir.create(out_fig_path, FALSE, TRUE)
  
  barplot_fl_nm <- paste0("proportional_reduction_in_", my_var_name, "_", intervention_name, ".png")
  
  png(file.path(out_fig_path, barplot_fl_nm),
      width = 17,
      height = 9,
      units = "cm",
      pointsize = 12,
      res = 300)
  
  print(p)
  
  dev.off()
  
}
