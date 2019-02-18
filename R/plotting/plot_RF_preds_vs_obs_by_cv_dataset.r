RF_preds_vs_obs_plot_stratif <- function(df, x, y, facet_var, file_name, file_path) {
  
  # browser()
  
  x_values <- pretty(df[, x], n = 5)
  y_values <- pretty(df[, y], n = 5)
  min_x_value <- min(x_values)
  max_x_value <- max(x_values)
  min_y_value <- min(y_values)
  max_y_value <- max(y_values)
  
  corr_coeff <- ddply(df, 
                      as.formula(paste0("dataset ~", facet_var)), 
                      calculate_wgt_cor, x, y)
  
  facet_plot_names_x <- as_labeller(c(admin = "Level 1 administrative unit",
                                      cell = "20 km pixel"))

  facet_plot_names_y <- as_labeller(c(train = "Train set",
                                      test = "Test set"))
  
  p <- ggplot(df, aes_string(x = x, y = y)) +
    facet_grid(as.formula(paste0("dataset ~", facet_var)),
               labeller = labeller(dataset = facet_plot_names_y,
                                   scale = facet_plot_names_x)) + 
    geom_point(aes_string(x = x, y = y), size = 0.8) +
    geom_abline(slope = 1, intercept = 0, linetype = 2) +
    scale_x_continuous("Observations",  
                       limits = c(min_x_value, max_x_value), 
                       breaks = x_values,
                       labels = x_values) +
    scale_y_continuous("Predictions", 
                       #limits = c(min_y_value, max_y_value), 
                       breaks = y_values,
                       labels = y_values) +
    theme(axis.title.x = element_text(size = 12, margin = margin(t = 20)),
          axis.title.y = element_text(size = 12, margin = margin(r = 20)),
          axis.text.x = element_text(size = 10),
          axis.text.y = element_text(size = 10),
          plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
          strip.text.x = element_text(size = 12),
          strip.text.y = element_text(size = 12),
          plot.title = element_text(vjust = 0.5))

  p2 <- p +
    geom_text(data = corr_coeff, 
              aes(x = x_values[length(x_values)-1], 
                  y = min_y_value, 
                  hjust = 1, 
                  label = paste0("italic(r) == ", V1)),
              parse = TRUE,
              inherit.aes = FALSE,
              size = 4) +
    facet_grid(as.formula(paste0("dataset ~", facet_var)),
               labeller = labeller(dataset = facet_plot_names_y,
                                   scale = facet_plot_names_x))
  
  dir.create(file_path, FALSE, TRUE)
  
  png(filename = file.path(file_path, file_name), 
      width = 16, 
      height = 12, 
      units = "cm", 
      pointsize = 12,
      res = 200)
  
  print(p2)
  
  dev.off()
}
