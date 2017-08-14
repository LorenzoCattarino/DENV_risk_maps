# Creates and saves one plot for each of three diagnostics 
# of the EM algorithm output:
# 1) pixel level sum of squares
# 2) admin unit levele sum of square
# 3) mean square error of the RF object

library(ggplot2)


# ---------------------------------------- define parameters


diagnostic_vars <- c("RF_ms_i", "ss_i", "ss_j")

strip_labs <- c(
  "internal RF mean square error", 
  "pixel level sum of square", 
  "admin unit level sum of square") 

strip_labs <- gsub('([[:punct:]])|\\s+','_', strip_labs)

diag_t_pth <- file.path("output", "EM_algorithm", "diagnostics", "best_model_20km_cw")

fig_file_tag <- paste0(strip_labs, ".png")
  
figure_out_path <- file.path("figures", "EM_algorithm", "best_model_20km_cw", "diagnostics")


# ---------------------------------------- get results 


fi <- list.files(diag_t_pth, 
                 pattern = ".*.rds",
                 full.names = TRUE)


EM_alg_run <- lapply(fi, readRDS) 


# ---------------------------------------- plot 


data_to_plot <- as.data.frame(EM_alg_run[[1]])

data_to_plot$iter <- seq_len(nrow(data_to_plot))

dir.create(figure_out_path, FALSE, TRUE)

for (i in seq_along(strip_labs)){
  
  png(file.path(figure_out_path, fig_file_tag[i]), 
      width = 5, height = 4.5, units = "in",
      res = 300)
  
  print(ggplot(data_to_plot, aes(iter, get(diagnostic_vars[i]))) +
  geom_line() +
  scale_x_continuous("Iterations") +
  scale_y_continuous(strip_labs[i]) +
  theme(axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12)))

  dev.off()

}
