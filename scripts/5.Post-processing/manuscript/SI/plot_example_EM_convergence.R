
library(ggplot2)


# define parameters ----------------------------------------------------------- 


var_to_fit <- "FOI"

j <- 1 # sample to plot

grid_size <- 1

diagnostic_vars <- c("ss_i", "ss_j", "RF_ms_i")

strip_labs <- c("20 km-level sum of square",
                "Administrative unit-level sum of square",
                "Internal mean square error")


# define variables ------------------------------------------------------------


model_type <- paste0(var_to_fit, "_boot_model")

my_dir <- paste0("grid_size_", grid_size)

diag_t_pth <- file.path("output", 
                        "EM_algorithm", 
                        "bootstrap_models", 
                        my_dir, 
                        model_type, 
                        "diagnostics")

fig_file_tag <- "EM_example_convergence.png"

figure_out_path <- file.path("figures", 
                             my_dir, 
                             model_type, 
                             "diagnostics",
                             paste0("sample_", j))


# get results ----------------------------------------------------------------- 


fi <- list.files(diag_t_pth, pattern = ".*.rds", full.names = TRUE)

EM_alg_run <- lapply(fi, readRDS) 


# plot ------------------------------------------------------------------------  


one_data_set <- EM_alg_run[[j]]

data_to_plot <- as.data.frame(one_data_set)

data_to_plot$iter <- seq_len(nrow(data_to_plot))

data_to_plot_long <- reshape2::melt(data_to_plot, 
                                    id.vars = "iter", 
                                    measure.vars = diagnostic_vars)

dir.create(my_path, FALSE, TRUE)

my_labs <- setNames(strip_labs,
                    diagnostic_vars)

p <- ggplot(data_to_plot_long, aes(iter, value)) +
  geom_line() +
  facet_wrap(~variable, 
             ncol = 1, 
             nrow = 3,
             scales = "free",
             strip.position = "top", 
             labeller = as_labeller(my_labs)) +
  scale_x_continuous("Iteration",
                     breaks = seq(10),
                     labels = seq(10),
                     limits = c(1,10)) +
  scale_y_continuous(NULL) +
  theme(axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12))

png(file.path(figure_out_path, fig_file_tag), 
    width = 10, 
    height = 15, 
    units = "cm",
    pointsize = 12,
    res = 200)

print(p)

dev.off()

