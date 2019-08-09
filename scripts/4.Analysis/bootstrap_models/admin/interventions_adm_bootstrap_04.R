# Plot proportional reduction in infections, cases and hospitalized cases 
# for vaccination when optimally picking screening age


library(dplyr)
library(ggplot2)

source(file.path("R", "utility_functions.R"))


# define parameters -----------------------------------------------------------


sf_vals <- c(0.7, 0.3)

screening_ages <- c(9, 16, 0)

leg_titles <- c(expression('R'['0']*' reduction'), "Screening age")

burden_measures <- c("infections", "cases", "hosp") 

y_axis_titles <- c("Reduction in infections", "Reduction in cases", "Reduction in hopsitalized cases")

out_fig_path <- file.path("figures", 
                          "predictions_world", 
                          "bootstrap_models",
                          "adm_1")

interventions <- c("wolbachia", "vaccine")


# define variables ------------------------------------------------------------


sf_vals_perc <- (1 - sf_vals) * 100

leg_labels <- list(paste0(sf_vals_perc, "%"), c("9", "16", "Optimal"))


# plotting ------------------------------------------------------------------


for (i in seq_along(interventions)) {
  
  intervention_name <- interventions[i]
  
  message(intervention_name)
  
  for (j in seq_along(burden_measures)) {
    
    my_var_name <- burden_measures[j]
    
    message(my_var_name)
    
    y_axis_title <- y_axis_titles[j]
    
    if(intervention_name == "wolbachia"){
      
      summary_table_orig <- read.csv(file.path("output", 
                                               "predictions_world", 
                                               "bootstrap_models",
                                               "adm_1",
                                               paste0("prop_change_", my_var_name, "_", intervention_name, ".csv")))
      
      summary_table <- subset(summary_table_orig, treatment %in% sf_vals & phi_set_id != "FOI")
      summary_table$treatment <- factor(summary_table$treatment, levels = sf_vals)
      
    } else {
      
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
      
      summary_table <- summary_table_orig
      summary_table$treatment <- factor(summary_table$treatment, levels = screening_ages)
      
      out_fl_nm <- sprintf("prop_change_%s_%s%s", my_var_name, intervention_name, ".csv")
      
      write_out_csv(summary_table, file.path("output", 
                                             "predictions_world", 
                                             "bootstrap_models",
                                             "adm_1"),
                    out_fl_nm,
                    row.names = FALSE)
      
    }
    
    y_values <- seq(0, 1, 0.2)
    
    scale_fill_vals <- c("lightskyblue1", "lightskyblue4")
    
    if(intervention_name == "vaccine"){
      
      y_values <- seq(0, 0.45, 0.1)
    
      scale_fill_vals <- c("lightskyblue1", "lightskyblue4", "deepskyblue")
      
    }
    
    p <- ggplot(summary_table, aes(x = treatment, y = mean, fill = treatment, ymin = lCI, ymax = uCI)) +
      geom_bar(stat = "identity", position = position_dodge(width = 0.5), width = 0.9) +
      geom_errorbar(width = .15, position = position_dodge(.9)) +
      facet_grid(. ~ phi_set_id) +
      scale_fill_manual(values = scale_fill_vals,
                        labels = leg_labels[[i]],
                        guide = guide_legend(title = leg_titles[i],
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
  
}

