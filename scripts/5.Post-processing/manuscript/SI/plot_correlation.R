
library(ggplot2)
library(plyr)
library(grid)
library(gridExtra)

source(file.path("R", "create_parameter_list.R"))
source(file.path("R", "random_forest", "reset_no_transmission.R"))
source(file.path("R", "plotting", "simple_corr_plot.R"))


# define parameters -----------------------------------------------------------  


extra_prms <- list(dependent_variable = "FOI")   


# define variables ------------------------------------------------------------


parameters <- create_parameter_list(extra_params = extra_prms)

out_fig_path <- file.path("figures", "EM_algorithm")


# load data ------------------------------------------------------------------- 


# FOI best fit (16 predictors)
fit_1 <- readRDS(file.path(file.path("output",
                                     "EM_algorithm",
                                     "best_fit_models",
                                     "model_1",
                                     "adm_foi_predictions"), 
                           "all_scale_predictions.rds"))

# FOI bootstrap fit (16 predictors, 5 degree and all data) 
fit_2 <- read.csv(file.path(file.path("output",
                                      "EM_algorithm",
                                      "bootstrap_models",
                                      "model_2",
                                      "scatter_plots"), 
                            "pred_vs_obs_plot_averages_all_data.csv"),
                  stringsAsFactors = FALSE)

# FOI bootstrap fit (16 predictors, 1/120 degree and all data)
fit_3 <- read.csv(file.path(file.path("output",
                                      "EM_algorithm",
                                      "bootstrap_models",
                                      "model_1",
                                      "scatter_plots"), 
                            "pred_vs_obs_plot_averages_all_data.csv"),
                  stringsAsFactors = FALSE)

# FOI bootstrap fit (16 predictors, 5 degree and no pseudo absence data) 
fit_4 <- read.csv(file.path(file.path("output",
                                      "EM_algorithm",
                                      "bootstrap_models",
                                      "model_2",
                                      "scatter_plots"), 
                            "pred_vs_obs_plot_averages_no_psAb.csv"),
                  stringsAsFactors = FALSE)

# FOI bootstrap fit (16 predictors, 1/120 degree and no pseudo absence data)
fit_5 <- read.csv(file.path(file.path("output",
                                      "EM_algorithm",
                                      "bootstrap_models",
                                      "model_1",
                                      "scatter_plots"), 
                            "pred_vs_obs_plot_averages_no_psAb.csv"),
                  stringsAsFactors = FALSE)


# with pseudo absences --------------------------------------------------------

# best fit
# dts_1 <- dplyr::rename(fit_1, cell = mean_p_i)
dts_1 <- fit_1

# 5 degrees, in-sample
dts_2 <- subset(fit_2, dataset == "train")

# 5 degrees, out-of-sample
dts_3 <- subset(fit_2, dataset == "test")

# 1/120 degrees, in-sample
dts_4 <- subset(fit_3, dataset == "train")

# 1/120 degrees, out-of-sample
dts_5 <- subset(fit_3, dataset == "test")

all_dts <- list(dts_1, dts_2, dts_3, dts_4, dts_5)

all_dts <- lapply(all_dts, reset_no_transmission, parameters)

all_plots <- lapply(seq_along(all_dts), wrapper_simple_corr_plot, all_dts)
  
dir.create(out_fig_path, FALSE, TRUE)

png(filename = file.path(out_fig_path, "correlation_set_all_data.png"), 
    width = 16, 
    height = 15, 
    units = "cm", 
    pointsize = 12,
    res = 200)

grid.arrange(grobs = all_plots, 
             ncol = 2,
             bottom = textGrob("Observations", gp = gpar(fontsize = 15)),
             left = textGrob("Predictions", gp = gpar(fontsize = 15), rot = 90))

dev.off()


# without pseudo absences -----------------------------------------------------


dt_typ <- c("serology", "caseReport")
  
dts_1 <- dts_1[dts_1$type %in% dt_typ, ]
dts_2 <- subset(fit_4, dataset == "train")
dts_3 <- subset(fit_4, dataset == "test")
dts_4 <- subset(fit_5, dataset == "train")
dts_5 <- subset(fit_5, dataset == "test")

all_dts <- list(dts_1, dts_2, dts_3, dts_4, dts_5)

all_dts <- lapply(all_dts, reset_no_transmission, parameters)

all_plots <- lapply(seq_along(all_dts), wrapper_simple_corr_plot, all_dts)

dir.create(out_fig_path, FALSE, TRUE)

png(filename = file.path(out_fig_path, "correlation_set_no_psAb.png"), 
    width = 16, 
    height = 15, 
    units = "cm", 
    pointsize = 12,
    res = 200)

grid.arrange(grobs = all_plots, 
             ncol = 2,
             bottom = textGrob("Observations", gp = gpar(fontsize = 15)),
             left = textGrob("Predictions", gp = gpar(fontsize = 15), rot = 90))

dev.off()
