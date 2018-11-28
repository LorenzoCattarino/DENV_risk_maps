
# create barplots of total numbers for Wolbachia impact

library(ggplot2)


# define parameters -----------------------------------------------------------


parameters <- list(
  burden_measures = c("infections", "cases", "hosp"),
  intervention_name = "wolbachia",
  desired_n_int = c(8, 8, 5))   

out_fig_path <- file.path("figures", 
                          "predictions_world", 
                          "bootstrap_models",
                          "adm_1")


# define variables ------------------------------------------------------------


intervention_name <- parameters$intervention_name

desired_n_int <- parameters$desired_n_int

burden_measures <- parameters$burden_measures


# plotting --------------------------------------------------------------------


for (i in seq_along(burden_measures)){
  
  my_var_name <- burden_measures[i]
  
  summary_tab_fl_nm <- paste0("total_", 
                              my_var_name, 
                              "_", 
                              intervention_name, 
                              ".csv")
  
  summary_table <- read.csv(file.path("output", 
                                      "predictions_world", 
                                      "bootstrap_models",
                                      "adm_1",
                                      summary_tab_fl_nm))
  
  treatment_levels <- unique(summary_table$treatment)
  
  summary_table[, "treatment"] <- factor(summary_table[, "treatment"],
                                         levels = treatment_levels,
                                         labels = treatment_levels)
  
  y_values <- pretty(summary_table$mean, desired_n_int[i])
  max_y_value <- max(summary_table$uCI)
  
  p <- ggplot(summary_table, aes(treatment, mean, fill = treatment, ymin = lCI, ymax = uCI)) +
    geom_bar(stat = "identity", position = "dodge", width = 1) +
    geom_errorbar(width = .25, position = position_dodge(.9)) +
    facet_grid(. ~ phi_set_id) +
    scale_fill_manual(values = c("lightskyblue1", "lightskyblue3", "lightskyblue4"),
                      labels = c("0%", "50%", "70%"),
                      guide = guide_legend(title = expression('R'['0']*' reduction'),
                                           keywidth = 2,
                                           keyheight = 2)) +
    xlab(NULL) +
    scale_y_continuous("Mean (95% CI)",
                       breaks = y_values,
                       labels = format(y_values/1000000),
                       limits = c(min(y_values), max_y_value)) +
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.y = element_text(size = 12),
          plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
          strip.text.x = element_text(size = 8))
  
  dir.create(out_fig_path, FALSE, TRUE)
  
  barplot_fl_nm <- paste0("total_", my_var_name, "_", intervention_name, ".png")
  
  png(file.path(out_fig_path, barplot_fl_nm),
      width = 17,
      height = 7,
      units = "cm",
      pointsize = 12,
      res = 300)
  
  print(p)
  
  dev.off()
  
}
