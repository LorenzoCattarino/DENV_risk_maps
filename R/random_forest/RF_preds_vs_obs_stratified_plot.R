RF.preds.vs.obs.plot.stratif.no.labels <- function (
  run_id, exp_id, diagnostics, predictions, my_path) {

  dir.create(my_path, FALSE, TRUE)

  corr.coeff.train <- diagnostics["corr_coeff_train"]
  corr.coeff.valid <- diagnostics["corr_coeff_valid"]
  
  y_values <- pretty(predictions$value)
  max_y_value <- max(y_values)
  min_y_value <- min(y_values) 
  
  x_values <- pretty(predictions$y.data)
  max_x_value <- max(x_values)
  min_x_value <- min(x_values)
  
  file_name <- sprintf("pred_vs_obs_plot_%s_%s%s", 
                       paste("exp", exp_id, sep = "_"), 
                       paste("run", run_id, sep = "_"), ".tiff")
  
  tiff(filename = file.path(my_path, file_name), 
       width = 10, height = 6, units = "in", compression = "lzw", res = 200)

  print(ggplot(predictions, aes(x = y.data, y = value)) +
          facet_grid(. ~ dataset) + 
          geom_point(aes(x = y.data, y = value), size = 1) +
          geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
          scale_x_continuous("Observations", 
                             limits = c(min_x_value, max_x_value), 
                             breaks = x_values,
                             labels = x_values) +
          scale_y_continuous("Predictions", 
                             limits = c(min_y_value, max_y_value), 
                             breaks = y_values) +
          theme(axis.title.x = element_text(size = 15, margin = margin(t = 20)),
                axis.title.y = element_text(size = 15, margin = margin(r = 20)),
                axis.text.x = element_text(size = 15),
                axis.text.y = element_text(size = 15),
                plot.margin = unit(c(1,1,1,1), "cm"),
                strip.text.x = element_text(size = 12)) +
          geom_text(data = data.frame(x = min_x_value + 0.015, 
                                      y = max_y_value - 0.01, 
                                      label = round(c(corr.coeff.train, corr.coeff.valid), 3), 
                                      dataset = c("train_set","test_set")), 
                    aes(x, y, label = paste("Corr coeff = ", label, sep = "")), size = 5))
  
  dev.off()
}

# RF.preds.vs.obs.plot.stratif <- function(df_to_plot, list_result, title_character_string)
# {
#   ID.exp <- unique(df_to_plot$ID.exp)
#   
#   dir.create(file.path("figures", "dengue_dataset", "FOI_predictions", 
#                        paste("exp", ID.exp, sep = "_")),
#              FALSE, TRUE)
#   
#   diagnostics <- list_result[[1]]
#   corr.coeff.train <- diagnostics["corr.coeff.train"]
#   corr.coeff.valid <- diagnostics["corr.coeff.valid"]
#   
#   result_df_wide <- list_result[[2]]
#   
#   result_df_long <- melt(result_df_wide, id.vars=c("country_code", "admin1", "y.data"), variable.name="dataset")
#   
#   y_values <- pretty(result_df_long$value)
#   max_y_value <- max(y_values)
#   min_y_value <- min(y_values) 
#   
#   x_values <- pretty(result_df_long$y.data)
#   max_x_value <- max(x_values)
#   min_x_value <- min(x_values)
#   
#   stratif_pred_vs_obs_plot <- ggplot(result_df_long, aes(x = y.data, y = value, 
#                                                          label = paste(country_code, admin1, sep="_"))) +
#     facet_grid(. ~ dataset) +
#     ggtitle("Predicted vs observed FOI values") + 
#     geom_point(aes(x=y.data, y=value), size=1) +
#     geom_abline(intercept = 0, slope=1, linetype = "dashed") +
#     geom_text(data=subset(result_df_long, (y.data < 0.03 | y.data > 0.05)), hjust = 0, nudge_x = 0.001, size = 1.5) +
#     scale_x_continuous("Observations", 
#                        limits=c(min_x_value, max_x_value), 
#                        breaks=x_values,
#                        labels=x_values) +
#     scale_y_continuous("Predictions", 
#                        limits=c(min_y_value, max_y_value), 
#                        breaks=y_values) +
#     theme(axis.title.x = element_text(hjust=0.5, vjust=1),
#           axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0),
#           axis.title.y = element_text(hjust=0.5, vjust=0),
#           axis.text=element_text(size=12),
#           plot.margin=unit(c(1,1,0.3,1), "cm"),
#           plot.title = element_text(lineheight=1, face="bold"))
#   
#   ggsave(stratif_pred_vs_obs_plot, file=file.path("figures", "dengue_dataset", "neg_FOI_values_test", 
#                                                   sprintf("pred_vs_obs_plot_%s%s", title_character_string, ".tiff")), 
#          width = 10, height = 6, units = "in", dpi = 200)
# }
