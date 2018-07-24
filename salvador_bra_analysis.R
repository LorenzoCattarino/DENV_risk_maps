
library(fields) # for image.plot()
library(RColorBrewer) # for brewer.pal()


# define parameters -----------------------------------------------------------


parameters <- list(
  dependent_variable = "FOI",
  shape_1 = 0,
  shape_2 = 5,
  shape_3 = 1.6e6,
  pseudoAbs_value = -0.02,
  foi_offset = 0.03,
  no_trees = 500,
  min_node_size = 20,
  all_wgt = 1,
  wgt_limits = c(1, 500),
  EM_iter = 10,
  no_predictors = 26) 

my_col <- rev(colorRampPalette(brewer.pal(11, "RdBu"))(100))

var_to_fit <- parameters$dependent_variable

number_of_predictors <- parameters$no_predictors

out_path <- file.path("figures", 
                      "env_variables",
                      "salvador")

res <- (1 / 120) * 20
  
model_type_tag <- "_best_model_3"


# define variables ------------------------------------------------------------


model_type <- paste0(var_to_fit, model_type_tag)


# load data -------------------------------------------------------------------


pxl_data <- readRDS(file.path("output", 
                              "EM_algorithm", 
                              "best_fit_models",
                              paste0("env_variables_", var_to_fit, "_fit"), 
                              "covariates_and_foi_20km.rds"))

predictor_rank <- read.csv(file.path("output", 
                                     "variable_selection",
                                     "stepwise",
                                     "predictor_rank.csv"), 
                           stringsAsFactors = FALSE)

sqr_pred_data <- readRDS(file.path("output", 
                                   "EM_algorithm", 
                                   "best_fit_models", 
                                   model_type, 
                                   "square_predictions_all_data.rds"))


# pre process -----------------------------------------------------------------


pxl_data$p_i <- sqr_pred_data

salvador_window <- subset(pxl_data, latitude >= -14 & latitude <= -11 & longitude >= -40 & longitude <= -37)

tile_set <- salvador_window

my_predictors <- predictor_rank$name[1:number_of_predictors]

my_predictors <- c(my_predictors, "p_i")

dir.create(out_path, FALSE, TRUE)


# plot covariates -------------------------------------------------------------


file_name <- "salvador_predictors.png"

png(file.path(out_path, file_name), 
    width = 20, 
    height = 20, 
    units = "cm", 
    res = 300)

par(mfrow = c(6,5), mar = c(2,2,2,5))

for (i in seq_along(my_predictors)){
  
  var <- my_predictors[i]
    
  message(var)
  
  lons <- seq(round(2 * min(tile_set$longitude) / res), round(2 * max(tile_set$longitude) / res), by = 2) * res / 2
  lats <- seq(round(2 * min(tile_set$latitude) / res), round(2 * max(tile_set$latitude) / res), by = 2) * res/2
  dat.mat <- matrix(NA, nrow = length(lons), ncol = length(lats))
  mm.lons <- match(round(2 * tile_set$longitude / res), round(2 * lons / res))
  mm.lats <- match(round(2 * tile_set$latitude / res), round(2 * lats / res))
  
  dat.mat[cbind(mm.lons, mm.lats)] <- tile_set[, var]
  
  image.plot(lons, 
             lats, 
             dat.mat, 
             asp = 1, 
             col = my_col, 
             main = var,
             xlab = "", 
             ylab = "", 
             legend.shrink = 0.7, 
             legend.width = 1)
  
}

dev.off()


# plot histogram --------------------------------------------------------------


file_name <- "salvador_rfd.png"

png(file.path(out_path, file_name), 
    width = 20, 
    height = 20, 
    units = "cm", 
    res = 300)

par(mfrow = c(6,5), mar = c(2,2,2,2))

for (i in seq_along(my_predictors)){
  
  var <- my_predictors[i]
  
  hist(pxl_data[, var], main = var)
  
  hot_spot <- pxl_data[229595, var]
  
  abline(v = hot_spot, col = "red")

}

dev.off()
