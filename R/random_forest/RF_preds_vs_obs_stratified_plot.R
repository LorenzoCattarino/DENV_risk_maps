RF_preds_vs_obs_plot_stratif <- function (
  df, x, y, facet_var, file_name, file_path) {
  
  # y_values <- pretty(df[, y])
  # max_y_value <- max(y_values)
  # min_y_value <- min(y_values) 
  # 
  # x_values <- pretty(df[, x])
  # max_x_value <- max(x_values)
  # min_x_value <- min(x_values)
  
  #browser()
  
  min_x_value <- 0
  max_x_value <- 0.07
  res <- 0.01
  
  x_values <- seq(min_x_value, max_x_value, res)
  
  p <- ggplot(df, aes_string(x = x, y = y)) +
    facet_grid(as.formula(paste0("dataset ~", facet_var))) + 
    geom_point(aes_string(x = x, y = y), size = 1) +
    #geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
    scale_x_continuous("Observations",    # expression("Observations " ~ (10^{-1}))
                       limits = c(min_x_value, max_x_value), 
                       breaks = x_values,
                       labels = x_values) +
    scale_y_continuous("Predictions", 
                       limits = c(min_x_value, max_x_value), 
                       breaks = x_values,
                       labels = x_values) +
    theme(axis.title.x = element_text(size = 15, margin = margin(t = 20)),
          axis.title.y = element_text(size = 15, margin = margin(r = 20)),
          axis.text.x = element_text(size = 11),
          axis.text.y = element_text(size = 11),
          plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
          strip.text.x = element_text(size = 12),
          strip.text.y = element_text(size = 12))
    # geom_text(data = data.frame(x = min_x_value + 0.015, 
    #                             y = max_y_value - 0.01, 
    #                             label = round(c(corr.coeff.train, corr.coeff.valid), 3), 
    #                             dataset = c("train_set","test_set")), 
    #           aes(x, y, label = paste("Corr coeff = ", label, sep = "")), size = 5)
  
  p2 <- p + geom_smooth(method = "lm", formula = y ~ x - 1, se = FALSE)
  
  eq <- ddply(df, as.formula(paste0("dataset ~", facet_var)), lm_eqn, y = y, x = x)
  
  p3 <- p2 + 
    geom_text(data = eq, aes(x = 0.04, y = max_x_value, label = V1), 
              parse = TRUE, 
              inherit.aes = FALSE) +
    facet_grid(as.formula(paste0("dataset ~", facet_var)))
  
  dir.create(file_path, FALSE, TRUE)
  
  png(filename = file.path(file_path, file_name), 
      width = 10, 
      height = 6, 
      units = "in", 
      pointsize = 12,
      res = 200)
  
  print(p3)
  
  dev.off()
}
