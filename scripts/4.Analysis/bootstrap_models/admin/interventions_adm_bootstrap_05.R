# Plot proportional reduction in infections, cases and hospitalized cases 
# for vaccination


library(dplyr)
library(ggplot2)

source(file.path("R", "utility_functions.R"))


# define parameters -----------------------------------------------------------


screening_ages <- c(9, 16, 0)

leg_title <- "Screening age"

leg_labels <- c("9", "16", "Optimal")

burden_measures <- c("infections", "cases", "hosp") 

y_axis_titles <- c("Reduction in infections", 
                   "Reduction in cases", 
                   "Reduction in hopsitalized cases")

out_fig_path <- file.path("figures", 
                          "predictions_world", 
                          "bootstrap_models")

intervention_name <- "vaccine"


# plotting ------------------------------------------------------------------



for (j in seq_along(burden_measures)) {
  
  my_var_name <- burden_measures[j]
  
  message(my_var_name)
  
  y_axis_title <- y_axis_titles[j]
  
  summary_table_orig_mean <- read.csv(file.path("output", 
                                                "predictions_world", 
                                                "bootstrap_models",
                                                paste0("prop_change_", my_var_name, "_mean_", intervention_name, ".csv")))
  
  summary_table_orig_L95 <- read.csv(file.path("output", 
                                               "predictions_world", 
                                               "bootstrap_models",
                                               paste0("prop_change_", my_var_name, "_L95_", intervention_name, ".csv")))
  
  summary_table_orig_U95 <- read.csv(file.path("output", 
                                               "predictions_world", 
                                               "bootstrap_models",
                                               paste0("prop_change_", my_var_name, "_U95_", intervention_name, ".csv")))
  
  summary_table_orig <- summary_table_orig_mean
  
  summary_table_orig$lCI <- summary_table_orig_L95$mean - (1.92 * summary_table_orig$sd)
  
  summary_table_orig$uCI <- summary_table_orig_U95$mean + (1.92 * summary_table_orig$sd)
  
  summary_table <- summary_table_orig
  summary_table$treatment <- factor(summary_table$treatment, 
                                    levels = screening_ages)
  
  
  # for optimal age analysis
  
  summary_table_orig_mean <- read.csv(file.path("output", 
                                                "predictions_world", 
                                                "bootstrap_models",
                                                "adm_1",
                                                paste0("prop_change_", my_var_name, "_mean_", intervention_name, ".csv")))
  
  summary_table_orig_L95 <- read.csv(file.path("output", 
                                               "predictions_world", 
                                               "bootstrap_models",
                                               "adm_1",
                                               paste0("prop_change_", my_var_name, "_L95_", intervention_name, ".csv")))
  
  summary_table_orig_U95 <- read.csv(file.path("output", 
                                               "predictions_world", 
                                               "bootstrap_models",
                                               "adm_1",
                                               paste0("prop_change_", my_var_name, "_U95_", intervention_name, ".csv")))
  
  summary_table_orig <- summary_table_orig_mean
  
  summary_table_orig$lCI <- summary_table_orig_L95$mean - (1.92 * summary_table_orig$sd)
  
  summary_table_orig$uCI <- summary_table_orig_U95$mean + (1.92 * summary_table_orig$sd)
  
  summary_table_adm <- summary_table_orig
  summary_table_adm$treatment <- factor(summary_table_adm$treatment, 
                                        levels = screening_ages)
  
  
  summary_table_all <- rbind(summary_table, summary_table_adm)
  summary_table_all$treatment <- factor(summary_table_all$treatment, 
                                        levels = screening_ages)

    
  # save
  
  
  out_fl_nm <- sprintf("prop_change_%s_%s%s", my_var_name, intervention_name, ".csv")
  
  write_out_csv(summary_table_all, file.path("output", 
                                             "predictions_world", 
                                             "bootstrap_models"),
                out_fl_nm,
                row.names = FALSE)
  
  # plot
  
  
  y_values <- seq(0, 0.45, 0.1)
  
  scale_fill_vals <- c("lightskyblue1", "lightskyblue4", "deepskyblue")
  
  p <- ggplot(summary_table_all, aes(x = treatment, y = mean, fill = treatment, ymin = lCI, ymax = uCI)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.5), width = 0.9) +
    geom_errorbar(width = .15, position = position_dodge(.9)) +
    facet_grid(. ~ phi_set_id) +
    scale_fill_manual(values = scale_fill_vals,
                      labels = leg_labels,
                      guide = guide_legend(title = leg_title,
                                           keywidth = 1.3,
                                           keyheight = 1.3,
                                           label.theme = element_text(size = 12))) +
    xlab(NULL) +
    scale_y_continuous(y_axis_title,
                       breaks = y_values,
                       labels = paste0(y_values * 100, "%"),
                       limits = c(min(y_values), max(y_values) + .05),
                       expand = expand_scale(mult = c(0, .05))) +
    theme_bw() +
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.y = element_text(size = 12),
          plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
          strip.text.x = element_text(size = 10))
  
  dir.create(out_fig_path, FALSE, TRUE)
  
  barplot_fl_nm <- paste0("proportional_reduction_in_", my_var_name, "_", intervention_name, ".png")
  
  png(file.path(out_fig_path, barplot_fl_nm),
      width = 15,
      height = 9,
      units = "cm",
      pointsize = 12,
      res = 300)
  
  print(p)
  
  dev.off()
  
}


