plot_EM_diagnostics <- function(my_data, 
                                out_path,
                                out_name){
  
  diagnostic_vars <- c("RF_ms_i", "ss_i", "ss_j", "r_av_sqr", "r_adm")
  
  strip_labs <- c("mean square error", 
                  "1/6 degree sum of square", 
                  "admin unit sum of square",
                  "1/6 degree correlation",
                  "admin unit correlation") 
  
  names(strip_labs) <- diagnostic_vars
  
  data_to_plot <- as.data.frame(my_data)
  
  data_to_plot$iter <- seq_len(nrow(data_to_plot))
  
  data_to_plot_long <- reshape2::melt(data_to_plot, 
                                      id.vars = "iter", 
                                      measure.vars = diagnostic_vars)
  
  dir.create(out_path, FALSE, TRUE)
  
  p <- ggplot(data_to_plot_long, aes(iter, value)) +
    geom_line() +
    scale_x_continuous("Iterations", breaks = seq_len(10), labels = seq_len(10)) +
    scale_y_continuous(NULL) +
    facet_wrap(~ variable, ncol = 2, scales = "free_y", labeller = labeller(variable = strip_labs)) +
    theme(axis.title.x = element_text(size = 12),
          axis.title.y = element_text(size = 12),
          axis.text.x = element_text(size = 10),
          axis.text.y = element_text(size = 10))
  
  png(file.path(out_path, out_name), 
      width = 13, 
      height = 13, 
      units = "cm",
      pointsize = 12,
      res = 200)
  
  print(p)
  
  dev.off()
  
}