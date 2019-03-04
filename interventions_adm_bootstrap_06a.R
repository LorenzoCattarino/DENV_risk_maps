# create barplots of total numbers for vaccine impact

library(ggplot2)

source(file.path("R", "utility_functions.R"))


# define parameters -----------------------------------------------------------


desired_n_int <- c(8, 6, 5)

intervention_name <- "vaccine" 

vacc_estimates = c("mean", "L95", "U95")

burden_measures <- c("infections", "cases", "hosp") 

out_fig_path <- file.path("figures", 
                          "predictions_world", 
                          "bootstrap_models",
                          "adm_1")


# load data ------------------------------------------------------------------- 


summary_table <- read.csv(file.path("output", 
                                    "predictions_world", 
                                    "bootstrap_models",
                                    "adm_1",
                                    paste0("total_", intervention_name, ".csv")))

summary_table_2 <- read.csv(file.path("output", 
                                      "predictions_world", 
                                      "bootstrap_models",
                                      "adm_1",
                                      paste0("prop_change_", intervention_name, ".csv")))


# plotting ------------------------------------------------------------------


for (j in seq_along(burden_measures)) {
  
  for (i in seq_along(vacc_estimates)) {
    
    bur_meas <- burden_measures[j]
    
    vacc_est <- vacc_estimates[i]
    
    summary_table_sub <- subset(summary_table, burden_measure == bur_meas & estimate == vacc_est)
    
    summary_table_2_sub <- subset(summary_table_2, burden_measure == bur_meas & estimate == vacc_est)
    summary_tab_fl_nm <- paste0("prop_change_", bur_meas, "_", vacc_est, "_", intervention_name, ".csv")
    write_out_csv(summary_table_2_sub, file.path("output", 
                                                 "predictions_world", 
                                                 "bootstrap_models",
                                                 "adm_1"), 
                  summary_tab_fl_nm)
    
    y_values <- pretty(0:max(summary_table_sub$mean), desired_n_int[j])
    max_y_value <- max(summary_table_sub$uCI)
    
    p <- ggplot(summary_table_sub, aes(bur_meas, mean, ymin = lCI, ymax = uCI)) + 
      geom_bar(stat = "identity", position = "dodge", width = 0.5, fill = "lightskyblue3") +
      geom_errorbar(width = .1, position = position_dodge(.9)) +
      facet_grid(. ~ phi_set_id) +
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
    
    barplot_fl_nm <- paste0("total_", bur_meas, "_", vacc_est, "_", intervention_name, ".png")
    
    png(file.path(out_fig_path, barplot_fl_nm),
        width = 17,
        height = 7,
        units = "cm",
        pointsize = 12,
        res = 300)
    
    print(p)
    
    dev.off()
    
  }
  
}
