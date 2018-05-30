# Creates and saves one plot for each of three diagnostics 
# of the EM algorithm output:
#
# 1) pixel level sum of squares
# 2) admin unit levele sum of square
# 3) mean square error of the RF object

library(ggplot2)


# define parameters ----------------------------------------------------------- 


parameters <- list(
  dependent_variable = "R0_3")   

diagnostic_vars <- c("RF_ms_i", "ss_i", "ss_j", "r_av_sqr", "r_adm")

strip_labs <- c("mean square error", 
                "pixel level sum of square", 
                "admin unit level sum of square",
                "pixel level correlation",
                "admin unit level correlation") 

names(strip_labs) <- diagnostic_vars


# define variables ------------------------------------------------------------


model_type <- paste0(parameters$dependent_variable, "_best_model")

# strip_labs <- gsub('([[:punct:]])|\\s+','_', strip_labs)

diag_t_pth <- file.path("output", 
                        "EM_algorithm", 
                        "best_fit_models",
                        model_type, 
                        "diagnostics")

fig_file_tag <- "diagnostics.png"
  
figure_out_path <- file.path("figures", 
                             "EM_algorithm", 
                             "best_fit_models",
                             model_type, 
                             "diagnostics")


# get results -----------------------------------------------------------------  


fi <- list.files(diag_t_pth, pattern = ".*.rds", full.names = TRUE)

EM_alg_run <- lapply(fi, readRDS) 


# plot ------------------------------------------------------------------------  


data_to_plot <- as.data.frame(EM_alg_run[[1]])

data_to_plot$iter <- seq_len(nrow(data_to_plot))

data_to_plot_long <- reshape2::melt(data_to_plot, 
                                    id.vars = "iter", 
                                    measure.vars = diagnostic_vars)

dir.create(figure_out_path, FALSE, TRUE)

png(file.path(figure_out_path, fig_file_tag), 
    width = 13, 
    height = 13, 
    units = "cm",
    res = 200)

print(ggplot(data_to_plot_long, aes(iter, value)) +
        geom_line() +
        scale_x_continuous("Iterations", breaks = seq_len(10), labels = seq_len(10)) +
        scale_y_continuous(NULL) +
        facet_wrap(~ variable, ncol = 2, scales = "free_y", labeller = labeller(variable = strip_labs)) +
        theme(axis.title.x = element_text(size = 12),
              axis.title.y = element_text(size = 12),
              axis.text.x = element_text(size = 10),
              axis.text.y = element_text(size = 10)))

dev.off()
