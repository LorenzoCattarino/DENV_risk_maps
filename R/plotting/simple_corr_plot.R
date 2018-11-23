simple_corr_plot <- function(i, df){
  
  x_values <- pretty(df[, "o_j"], n = 5)
  y_values <- pretty(df[, "admin"], n = 5)
  min_x_value <- min(x_values)
  max_x_value <- max(x_values)
  min_y_value <- min(y_values)
  max_y_value <- max(y_values)
  
  ggplot(df, aes(x = "o_j", y = "admin")) +
    geom_point(aes_string(x = "o_j", y = "admin"), size = 1) +
    geom_abline(slope = 1, intercept = 0, linetype = 2) +
    scale_x_continuous(limits = c(min_x_value, max_x_value), 
                       breaks = x_values,
                       labels = x_values) +
    scale_y_continuous(limits = c(min_y_value, max_y_value), 
                       breaks = y_values,
                       labels = y_values) +
    theme_bw(base_size = 11, base_family = "") +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_text(size = 11),
          axis.text.y = element_text(size = 11),
          plot.margin = unit(c(0, 0, 0, 0), "cm"),
          strip.text.x = element_text(size = 12),
          strip.text.y = element_text(size = 12)) +
    labs(tag = LETTERS[i])
  
}

wrapper_simple_corr_plot <- function(i, df_list) {
  
  df <- df_list[[i]]
  
  simple_corr_plot(i, df)
  
}

reset_pse_abs <- function(x, parameters){
  psAbs_val <- parameters$pseudoAbs_value
  x[, c("o_j", "admin", "cell")][x[, c("o_j", "admin", "cell")] < 1] <- psAbs_val
  x
}