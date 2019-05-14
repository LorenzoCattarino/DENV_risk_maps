
library(rgdal)
library(RColorBrewer)
library(colorRamps)
library(viridis)
library(fields)

source(file.path("R", "plotting", "functions_for_plotting_raster_maps.R"))
source(file.path("R", "prepare_datasets", "get_covariate_scaling_factor.R"))


# define parameters -----------------------------------------------------------


parameters <- list(
  grid_size = 1 / 120,       # decimal degrees
  resample_grid_size = 20)   # kms  

year.i <- 2007

year.f <- 2014

ppyear <- 64

n_pal_col <- 11 

req_n_col <- 100

altitude_var <- "altitude"

fourier_transform_elem <- "const_term"

FTs_dt <- c("DayTemp", "EVI", "MIR", "NightTemp", "RFE")

key_ttls <- c("Altitude", 
              "Diurnal temperature", 
              "EVI",
              "MIR",
              "Nocturnal temperature",
              "Precipitation",
              "Travel time",
              "Population density")


# define variables ------------------------------------------------------------


new_res <- parameters$grid_size * parameters$resample_grid_size
  
out_path <- file.path("figures", "env_variables", paste0(new_res, "_deg"))
  

# load data -------------------------------------------------------------------


all_sqr_covariates <- readRDS(file.path("output", 
                                        "env_variables", 
                                        "all_squares_env_var_0_1667_deg.rds"))

countries <- readOGR(dsn = file.path("output", "shapefiles"), 
                     layer = "gadm28_adm0_eras")


# pre processing --------------------------------------------------------------


lats <- seq(-90, 90, by = new_res)
lons <- seq(-180, 180, by = new_res)

all_combs_df <- expand.grid(FTs_dt, fourier_transform_elem)

all_combs_names <- apply(all_combs_df, 1, function(x) paste(x[1], x[2], sep = "_"))

my_predictors <- c(altitude_var, all_combs_names)

my_predictors <- c(my_predictors, "travel_time", "log_pop_den")

countries <- countries[!countries@data$NAME_ENGLI == "Antarctica", ]

#display.brewer.all()

alt_p <- terrain.colors(req_n_col)
daytemp_p <- rev(colorRampPalette(brewer.pal(n_pal_col, "RdBu"))(req_n_col)) 
EVI_p <- colorRampPalette(brewer.pal(n_pal_col, "Greens"))(req_n_col)  
MIR_p <- colorRampPalette(brewer.pal(n_pal_col, "Spectral"))(req_n_col) 
nighttemp_p <- rev(colorRampPalette(brewer.pal(n_pal_col, "RdYlBu"))(req_n_col))
RFE_p <- topo.colors(req_n_col)
ttime_p <- magma(req_n_col)
pop_den_p <- viridis(req_n_col)

palettes <- list(alt_p,
                 daytemp_p,
                 EVI_p,
                 MIR_p,
                 nighttemp_p,
                 RFE_p,
                 ttime_p,
                 pop_den_p)


# loop through the predictors to plot -----------------------------------------


for (i in seq_along(my_predictors)){
  
  my_pred <- my_predictors[i]
  
  scale <- get_covariate_scaling_factor(my_pred, ppyear, year.f, year.i)
    
  message(scale)
  
  if (my_pred == "travel_time") { 
    
    all_sqr_covariates[, my_pred] <- log(all_sqr_covariates[, my_pred])  
  
  }
  
  all_sqr_covariates[, my_pred] <- all_sqr_covariates[, my_pred] / scale
  
  out_fl_nm <- paste0(my_pred, ".png")
  
  pal <- palettes[[i]]
    
  key_title <- key_ttls[i]
    
  quick_raster_map(pred_df = all_sqr_covariates, 
                   statistic = my_pred, 
                   my_col = pal,
                   out_pt = out_path, 
                   out_name = out_fl_nm,
                   shp = countries,
                   key_ttle = key_title)
  
}
