
library(rgdal)
library(h2o)
library(ggplot2)
library(raster)
library(dplyr)
library(viridis)
library(colorRamps)
library(RColorBrewer)

source(file.path("R", "random_forest", "fit_h2o_RF_and_make_predictions.R"))
source(file.path("R", "prepare_datasets", "average_up.R"))
source(file.path("R", "plotting", "functions_for_plotting_square_level_maps.R"))
source(file.path("R", "utility_functions.R"))

my_h2o_ver <- "3.16.0.2"

if(packageVersion("h2o") != my_h2o_ver) install.packages(file.path("R_sources", "h2o_3.16.0.2.tar.gz"), repos = NULL, type = "source")
                 
                 
# define parameters -----------------------------------------------------------


parameters <- list(no_predictors = 9,
                   resample_grid_size = 20)

alpha_iso_code <- "BGD"
adm0 <- 20

pop_var <- "pop"
alt_var <- "altitude"
FT_elem <- c("const_term",	"Re0",	"Im0",	"Re1",	"Im1")
FT_var <- c("RFE", "DayTemp", "NightTemp", "EVI", "MIR")
LC_var <- paste("lct1_2012001", c(seq(0, 16, 1), 254, 255), sep = "_")

dts_out_pt <- file.path("output", "seroprevalence") 
  
bng_map_out_pt <- file.path("figures", "env_variables", "Bangladesh")
  
dts_out_nm <- "ProportionPositive_bangladesh_salje_env_var.csv"
  
my_col <- matlab.like(10)
my_col_cov = rev(colorRampPalette(brewer.pal(11, "RdBu"))(100))

year.i <- 2007
year.f <- 2014
ppyear <- 64


# load data -------------------------------------------------------------------
  
  
salje_data <- read.csv(file.path("data",
                                 "seroprevalence",
                                 "ProportionPositive_bangladesh_salje.csv"))

shp <- readOGR(file.path("data", "shapefiles", "BGD_adm_shp"), "BGD_adm1",
               stringsAsFactors = FALSE,
               integer64 = "allow.loss")

predictor_rank <- read.csv(file.path("output", 
                                     "variable_selection", 
                                     "metropolis_hastings", 
                                     "exp_1", 
                                     "variable_rank_final_fits_exp_1.csv"),
                           stringsAsFactors = FALSE)

age_distr <- read.csv(file.path("output", 
                                "datasets",
                                "country_age_structure.csv"), 
                      header = TRUE) 

pred <- readRDS(file.path("output",
                          "predictions_world", 
                          "best_fit_models",
                          "FOI_best_model",
                          "response.rds"))

covariates <- readRDS(file.path("output",
                                "env_variables",
                                "all_squares_env_var_0_1667_deg_dis.rds"))

foi_dataset <- read.csv(
  file.path("output", "foi", "All_FOI_estimates_linear_env_var_area.csv"),
  stringsAsFactors = FALSE) 


# pre processing --------------------------------------------------------------


gr_size <- parameters$resample_grid_size

res <- (1 / 120) * gr_size
lats <- seq(-90, 90, by = res)
lons <- seq(-180, 180, by = res)

shp_fort <- fortify(shp)


salje_data$id_point <- seq_len(nrow(salje_data))

salje_data$o_j <- salje_data$nPos / salje_data$nAll

names(covariates)[names(covariates) == "longitude"] <- "long.grid"
names(covariates)[names(covariates) == "latitude"] <- "lat.grid"

covariates$pop_density <- covariates$population / 342
covariates$pop_density <- log(1 + covariates$pop_density)

our_foi_point <- foi_dataset[foi_dataset$ID_0 == adm0, ]

salje_data$ISO <- alpha_iso_code


# plot the original seroprevalence points -------------------------------------


png(file.path("figures", "data", "salje_bangl_points_serop.png"),
    width = 12,
    height = 10,
    units = "cm",
    pointsize = 12,
    res = 300)

p <- ggplot() +
  geom_path(data = shp_fort, aes(x = long, y = lat, group = group), size = 0.3) +
  geom_point(data = salje_data, aes(x = lon, y = lat, colour = o_j), size = 2) +
  coord_equal() + 
  scale_color_viridis("seroprevalence") +
  theme_minimal()

print(p)

dev.off()

# old style!
#
# par(mar = c(0,0,0,0), oma = c(0,0,0,0))
# plot(shp)
# points(xy_spdf, pch = 21, cex = 0.6, bg = "red")
# text(location_xy$lon, 
#      location_xy$lat, 
#      labels = salje_data$id_point, 
#      pos = 3, 
#      cex = 0.5)


# crop the global prediction map ----------------------------------------------


pred_mat <- prediction_df_to_matrix(lats, lons, pred, "best")

pred_mat_ls <- list(x = lons,
                    y = lats,
                    z = pred_mat)

pred_r_mat <- raster(pred_mat_ls)

pred_r_mat_crp <- raster::crop(pred_r_mat, shp)

pred_r_mat_msk <- raster::mask(pred_r_mat_crp, shp)

pred_r_spdf <- as(pred_r_mat_msk, "SpatialPixelsDataFrame")

pred_r_df <- as.data.frame(pred_r_spdf)


# plot the cropped global prediction map --------------------------------------


png(file.path("figures", "data", "predicted_FOI_map.png"),
    width = 12,
    height = 10,
    units = "cm",
    pointsize = 12,
    res = 300)

p <- ggplot() +
  geom_tile(data = pred_r_df, aes(x = x, y = y, fill = layer)) +
  scale_fill_gradientn(colours = my_col, 
                       guide = guide_colourbar(title = "predicted FOI")) +
  geom_path(data = shp_fort, aes(x = long, y = lat, group = group), size = 0.3) +
  scale_x_continuous("long") +
  scale_y_continuous("lat") +
  coord_equal() +
  theme_minimal()

print(p)

dev.off()


# extract foi at the sero points coordinates ----------------------------------


my_points <- as.matrix(salje_data[,c("lon", "lat")])

cell_ids_in_raster <- cellFromXY(pred_r_mat, my_points)

raster_values <- pred_r_mat[cell_ids_in_raster]

salje_data$foi <- raster_values


# plot the 20 km foi at the sero points ---------------------------------------


png(file.path("figures", "data", "salje_bangl_points_20km_foi.png"),
    width = 12,
    height = 10,
    units = "cm",
    pointsize = 12,
    res = 300)

p <- ggplot() +
  geom_path(data = shp_fort, aes(x = long, y = lat, group = group), size = 0.3) +
  geom_point(data = salje_data, aes(x = lon, y = lat, colour = foi), size = 2) +
  coord_equal() + 
  scale_color_viridis("20 km FOI") +
  theme_minimal()

print(p)

dev.off()


# find admin unit 1 ------------------------------------------------------------------


location_xy <- salje_data[, c("lon", "lat")]
xy_spdf <- SpatialPoints(location_xy, proj4string = shp@proj4string)

overlay <- over(xy_spdf, shp)
country_numeric_code <- overlay$ID_0
adm_numeric_code <- overlay$ID_1
adm1_name <- overlay$NAME_1

salje_data$ID_0 <- country_numeric_code
salje_data$ID_1 <- adm_numeric_code
salje_data$adm1 <- adm1_name

salje_data <- subset(salje_data, !is.na(ID_1))


# save ------------------------------------------------------------------------


write_out_csv(salje_data, 
              dts_out_pt, 
              "ProportionPositive_bangladesh_salje_sqr_pred.csv")


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

for (j in seq_along(all_predictors)){
  
  my_pred <- all_predictors[j]
    
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
    theme_minimal()
  
  print(p)
  
  dev.off()
  
}


# # aggregate -------------------------------------------------------------------
# 
# 
# average_sqr <- average_up(salje_data_sqr, c("id_point", "ADM_0", "ADM_1"), "foi")
# 
# 
# # join adm averaged predictions to original data ------------------------------
# 
# 
# salje_data_2 <- left_join(salje_data, 
#                           average_sqr[, c("id_point", "foi")], 
#                           by = "id_point")
# 
# 
# # save ------------------------------------------------------------------------
# 
# 
# write_out_csv(salje_data_2, dts_out_pt, "ProportionPositive_bangladesh_salje_pred.csv")
# 
# 
# # seroprevalence --------------------------------------------------------------
# 
# 
# age_distr <- age_distr[setdiff(names(age_distr), c("band_80_99", "band_85_99"))]
# 
# age_band_tgs <- grep("band", names(age_distr), value = TRUE)
# 
# age_bounds_num <- sub("^[^_]+_", "", age_band_tgs)
# 
# age_bounds_num_2 <- sub("_", "-",age_bounds_num)
# 
# names(age_distr)[names(age_distr) %in% age_band_tgs] <- age_bounds_num_2
# 
# xx <- strsplit(age_bounds_num_2, "-")
# zz <- lapply(xx, as.numeric)
# yy <- vapply(zz, mean, numeric(1))
# 
# get_sero <- function(i, j){
#   1 - (exp(-4 * i * j))
# }
# 
# pred_serop <- t(vapply(salje_data_2$foi, get_sero, numeric(length(yy)), yy))
# 
# BGD_age_struct <- as.matrix(age_distr[age_distr$country == "Bangladesh", age_bounds_num_2])
# 
# BGD_age_structure_all_points <- matrix(rep(BGD_age_struct, 69), ncol = 20, byrow = TRUE)
# 
# mean_pred_serop <- rowSums(BGD_age_structure_all_points * pred_serop)
# 
# salje_data_2$p_j <- mean_pred_serop
# 
# salje_data_2$o_j <- salje_data_2$nPos / salje_data_2$nAll
# 
# corr_coeff <- cor.test(salje_data_2$o_j, salje_data_2$p_j)
# 
# ggplot(salje_data_2) +
#   geom_point(aes(x = o_j, y = p_j)) +
#   scale_x_continuous("observations", limits = c(0, 1)) + 
#   scale_y_continuous("predictions", limits = c(0, 1)) +
#   geom_text(aes(x = 0.75, y = 0, label = paste0("r = ", round(corr_coeff$estimate, 3))))
# 
# ggsave(file.path("figures", "bangladesh_seroprevalence_corr.png"))


# # extract env variables -------------------------------------------------------
# 
# 
# salje_data_ls <- df_to_list(salje_data, use_names = TRUE)
# 
# number_of_variables <- length(c(alt_var, LC_var)) + (length(c(pop_var, FT_elem)) * length(FT_var))
# 
# extracted_var_values <- sapply(
#   salje_data_ls, 
#   get_env_variables, 
#   no_vars = number_of_variables, 
#   pop_vars = pop_var,
#   alt_vars = alt_var, 
#   FT_elements = FT_elem, 
#   FT_data = FT_var, 
#   LC_vars = LC_var, 
#   admin_level = 1,
#   my_path = file.path("output", "env_variables"))
# 
# salje_data_2 <- cbind(salje_data, t(extracted_var_values))
# 
# colnames(salje_data_2) <- c(names(salje_data), alt_var, 
#                             apply(expand.grid(c(pop_var, FT_elem), FT_var), 
#                                   1, 
#                                   function(x) {paste(x[2],x[1], sep="_")}), LC_var)
# 
# 
# # save dataset ----------------------------------------------------------------
# 
# 
# write_out_csv(salje_data_2, dts_out_pt, dts_out_nm)
# 
# 
# # make predictions ------------------------------------------------------------
# 
# 
# salje_data$foi <- make_h2o_predictions(RF_obj, salje_data_2, my_predictors)
