generic_scatter_plot <- function(
  df_to_plot, x, y, 
  my_path, file_name, 
  x_axis_tag, y_axis_tag, ttl, 
  alpha, reverse_x_axis, mirror, fit_line, facet, facet_var){
  
  if(!is.null(alpha)){
    
    df_to_plot[, x] <- df_to_plot[, x] * alpha    
  
  }
  
  x_values <- pretty(df_to_plot[, x], n = 5) 
  
  if(mirror){
    y_values <- x_values
  }else{
    y_values <- pretty(df_to_plot[, y], n = 5)
  }
  
  dir.create(my_path, FALSE, TRUE)
  
  jpeg(filename = file.path(my_path, file_name), 
       width = 8, 
       height = 4, 
       units = "in", 
       res = 200)

  p1 <- ggplot(df_to_plot, aes(x = get(x), y = get(y))) +
    geom_point(aes(x = get(x), y = get(y)), size = 1)
  
  if(fit_line){
    p2 <- p1 + geom_smooth(method = "lm", formula = y ~ x)
    eq <- ddply(df_to_plot, as.formula(paste0("~", facet_var)), lm_eqn, y = y, x = x)
    p3 <- p2 + geom_text(data = eq, aes(x = 0.02, y = 0.06, label = V1), parse = TRUE, inherit.aes = FALSE) + 
      facet_wrap(as.formula(paste0("~", facet_var)))
  }else{
    p3 <- p1
  }
  
  if(facet){
    p4 <- p3 + facet_wrap(as.formula(paste0("~", facet_var)))
  }else{
    p4 <- p3
  }
  
  if(reverse_x_axis){
    p5 <- p4 + scale_x_reverse(x_axis_tag,
                               limits = c(max(x_values), min(x_values)),
                               breaks = rev(x_values),
                               labels = rev(x_values))
  }else{
    p5 <- p4 + scale_x_continuous(x_axis_tag,
                                  limits = c(min(x_values), max(x_values)),
                                  breaks = x_values,
                                  labels = x_values)
  }
    
  p6 <- p5 + scale_y_continuous(y_axis_tag, 
                                limits = c(min(y_values), max(y_values)), 
                                breaks = y_values) +
    ggtitle(ttl) +
    theme(axis.title.x = element_text(hjust = 0.5, vjust = 1, size = 15, margin = margin(t = 10)),
          axis.title.y = element_text(hjust = 0.5, vjust = 0, size = 15, margin = margin(r = 10)),
          axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 1, size = 15),
          axis.text.y = element_text(size = 15),
          plot.title = element_text(lineheight = 1, face = "bold", hjust = 0.5))
  
  print(p6)
  
  dev.off()
}
