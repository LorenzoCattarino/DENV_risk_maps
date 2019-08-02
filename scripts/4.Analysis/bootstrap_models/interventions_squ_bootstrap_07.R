# Plot proportional reduction in infections, cases and hospitalized cases 
# for wolbachia and vaccination 

library(dplyr)
library(ggplot2)


# define parameters -----------------------------------------------------------


sf_vals <- c(0.7, 0.3)

leg_titles <- c(expression('R'['0']*' reduction'), "Screening age")

burden_measures <- c("infections", "cases", "hosp") 

y_axis_titles <- c("Reduction in infections", "Reduction in cases", "Reduction in hopsitalized cases")

out_fig_path <- file.path("figures", 
                          "predictions_world", 
                          "bootstrap_models")

interventions <- c("wolbachia", "vaccine")


# define variables ------------------------------------------------------------


sf_vals_perc <- (1 - sf_vals) * 100

leg_labels <- list(paste0(sf_vals_perc, "%"), c("9", "16"))


# plotting ------------------------------------------------------------------


for (i in seq_along(interventions)) {
  
  for (j in seq_along(burden_measures)) {
    
    my_var_name <- burden_measures[j]
    
    intervention_name <- interventions[i]
    
    y_axis_title <- y_axis_titles[j]
    
    if(intervention_name == "wolbachia"){
      
      summary_table_orig <- read.csv(file.path("output", 
                                               "predictions_world", 
                                               "bootstrap_models",
                                               paste0("prop_change_", my_var_name, "_", intervention_name, ".csv")),
                                     header = TRUE)
      
      summary_table <- subset(summary_table_orig, treatment %in% sf_vals & phi_set_id != "FOI")
      summary_table$treatment <- factor(summary_table$treatment, levels = sf_vals)
      
    } else {
      
      summary_table_orig_mean <- read.csv(file.path("output", 
                                                    "predictions_world", 
                                                    "bootstrap_models",
                                                    paste0("prop_change_", my_var_name, "_mean_", intervention_name, ".csv")),
                                          header = TRUE)
      
      summary_table_orig_L95 <- read.csv(file.path("output", 
                                                   "predictions_world", 
                                                   "bootstrap_models",
                                                   paste0("prop_change_", my_var_name, "_L95_", intervention_name, ".csv")),
                                         header = TRUE)
      
      summary_table_orig_U95 <- read.csv(file.path("output", 
                                                   "predictions_world", 
                                                   "bootstrap_models",
                                                   paste0("prop_change_", my_var_name, "_U95_", intervention_name, ".csv")),
                                         header = TRUE)
      
      summary_table_orig <- summary_table_orig_mean
      
      summary_table_orig$lCI <- summary_table_orig_L95$mean - (1.92 * summary_table_orig$sd)
      
      summary_table_orig$uCI <- summary_table_orig_U95$mean + (1.92 * summary_table_orig$sd)
      
      summary_table <- summary_table_orig
      summary_table$treatment <- as.factor(summary_table$treatment)
      
    }
    
    y_values <- seq(0, 1, 0.2)
    
    if(intervention_name == "vaccine"){
      
      y_values <- seq(0, 0.4, 0.1)
    
    } 
    
    p <- ggplot(summary_table, aes(x = treatment, y = mean, fill = treatment, ymin = lCI, ymax = uCI)) +
      geom_bar(stat = "identity", position = position_dodge(width = 0.5), width = 0.9) +
      geom_errorbar(width = .25, position = position_dodge(.9)) +
      facet_grid(. ~ phi_set_id) +
      scale_fill_manual(values = c("lightskyblue1", "lightskyblue4"),
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
        width = 17,
        height = 9,
        units = "cm",
        pointsize = 12,
        res = 300)
    
    print(p)
    
    dev.off()
    
  }
  
}
