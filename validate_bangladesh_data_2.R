
# plot a map of each covariate with the Salje's points overlaid

library(rgdal)
library(ggplot2)
library(RColorBrewer)
library(viridis)

source(file.path("R", "plotting", "functions_for_plotting_square_level_maps.R"))
source(file.path("R", "utility_functions.R"))


# define parameters -----------------------------------------------------------


parameters <- list(resample_grid_size = 20)

year.i <- 2007
year.f <- 2014
ppyear <- 64

adm0 <- 20

dts_out_pt <- file.path("output", "seroprevalence", "salje") 

bng_map_out_pt <- file.path("figures", "env_variables", "Bangladesh")

gr_size <- parameters$resample_grid_size

res <- (1 / 120) * gr_size
lats <- seq(20, 27, by = res)
lons <- seq(88, 93, by = res)


# load data -------------------------------------------------------------------


shp <- readOGR(file.path("data", "shapefiles", "BGD_adm_shp"), "BGD_adm1",
               stringsAsFactors = FALSE,
               integer64 = "allow.loss")

covariates <- readRDS(file.path("output",
                                "env_variables",
                                "all_squares_env_var_0_1667_deg_dis.rds"))

predictor_rank <- read.csv(file.path("output", 
                                     "variable_selection", 
                                     "metropolis_hastings", 
                                     "exp_1", 
                                     "variable_rank_final_fits_exp_1.csv"),
                           stringsAsFactors = FALSE)

salje_data <- read.csv(file.path("output", 
                                 "seroprevalence",
                                 "salje",
                                 "predictions_20km.csv"),
                       stringsAsFactors = FALSE)

foi_dataset <- read.csv(file.path("output", 
                                  "foi", 
                                  "All_FOI_estimates_linear_env_var_area.csv"),
                        stringsAsFactors = FALSE) 


# pre process -----------------------------------------------------------------


shp_fort <- fortify(shp)

our_foi_point <- foi_dataset[foi_dataset$ID_0 == adm0, ]

names(covariates)[names(covariates) == "longitude"] <- "long.grid"
names(covariates)[names(covariates) == "latitude"] <- "lat.grid"

covariates$pop_density <- covariates$population / 342
covariates$pop_density <- log(1 + covariates$pop_density)

my_col_cov <- rev(colorRampPalette(brewer.pal(11, "RdBu"))(100))


# subset covariate dataset (bangladesh) ---------------------------------------


covariates_bgd <- covariates[covariates$ADM_0 == adm0, ]


# rescale covariate -----------------------------------------------------------


all_predictors <- predictor_rank$name

for (i in seq_along(all_predictors)){
  
  var <- all_predictors[i]
  
  scale <- 1
  
  if(grepl("Re.", var) | grepl("Im.", var)){
    
    scale <- ppyear * (year.f - year.i + 1) / 2 
    
  } 
  
  if(grepl("const_term$", var)){
    
    scale <- ppyear * (year.f - year.i + 1) 
    
  }  
  
  # message(scale)
  
  covariates_bgd[, var] <- covariates_bgd[, var] / scale
  
}


# map covariates --------------------------------------------------------------


all_predictors <- c(all_predictors, "pop_density")

dir.create(bng_map_out_pt, FALSE, TRUE)

n <- length(all_predictors)

out_cor_coef <- data.frame(predictor = rep(0, n), correlation = rep(0, n))
  
for (j in seq_along(all_predictors)){
  
  my_pred <- all_predictors[j]
  
  
  pred_mat <- prediction_df_to_matrix(lats, lons, covariates_bgd, my_pred)
  pred_mat_ls <- list(x = lons,
                      y = lats,
                      z = pred_mat)
  pred_r_mat <- raster::raster(pred_mat_ls)
  my_ext <- matrix(pred_r_mat@extent[], nrow = 2, byrow = TRUE)
  shp@bbox <- my_ext
  pred_r_mat_crp <- raster::crop(pred_r_mat, shp)
  pred_r_mat_msk <- raster::mask(pred_r_mat_crp, shp)
  pred_r_spdf <- as(pred_r_mat_msk, "SpatialPixelsDataFrame")
  pred_r_df <- as.data.frame(pred_r_spdf)
  my_points <- as.matrix(salje_data[, c("lon", "lat")])
  cell_ids_in_raster <- raster::cellFromXY(pred_r_mat, my_points)
  raster_values <- pred_r_mat[cell_ids_in_raster]
  corr_coeff <- round(cor(salje_data$o_j, raster_values), 3)
  
  out_cor_coef[j, "predictor"] <- my_pred
  out_cor_coef[j, "correlation"] <- corr_coeff
  
  png(file.path(bng_map_out_pt, paste0(j, "_", my_pred,".png")),
      width = 13,
      height = 10,
      units = "cm",
      pointsize = 12,
      res = 300)
  
  p <- ggplot() +
    geom_tile(data = covariates_bgd, aes_string(x = "long.grid", y = "lat.grid", fill = my_pred)) +
    geom_point(data = salje_data, aes(x = lon, y = lat, colour = o_j), size = 1.5) +
    geom_point(data = our_foi_point, aes(x = longitude, y = latitude), colour = "red", size = 2) +
    scale_fill_gradientn(colours = my_col_cov,
                         guide = guide_colourbar(title = my_pred)) +
    scale_color_viridis("seroprevalence") +
    geom_path(data = shp_fort, aes(x = long, y = lat, group = group), size = 0.3) +
    scale_x_continuous("longitude", limits = c(88, 93)) +
    scale_y_continuous("latitude", limits = c(20, 27), breaks = seq(20, 27, 1), labels = seq(20, 27, 1)) +
    coord_equal() +
    geom_text(aes(x = 88.5, y = 20.5, label = paste0("r = ", corr_coeff))) +
    theme_minimal()
  
  print(p)
  
  dev.off()
  
}

out_cor_coef <- out_cor_coef[order(out_cor_coef$correlation, decreasing = TRUE), ]


# save ------------------------------------------------------------------------


write_out_csv(out_cor_coef, dts_out_pt, "all_points_covariate_correlation.csv") 
