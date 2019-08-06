# Makes a map of mean

# 1) optimal vaccine impact age
# 2) seroprevalence at optimal vaccine impact age

source(file.path("R", "create_parameter_list.R"))
source(file.path("R", "plotting", "quick_polygon_map.R"))

library(sf)
library(ggplot2)


# define parameters -----------------------------------------------------------  


extra_prms <- list(id = 4,
                   statistic = "mean")


# define variables ------------------------------------------------------------


parameters <- create_parameter_list(extra_params = extra_prms)

model_type <- paste0("model_", parameters$id)

in_path <- file.path("output", 
                     "predictions_world",
                     "bootstrap_models",
                     model_type,
                     "adm_1")

out_pth <- file.path("figures", 
                     "predictions_world",
                     "bootstrap_models",
                     model_type,
                     "adm_1")

statistic <- parameters$statistic

vars_to_average <- c("I_num_1_max_age_vaccine_4",
                     "C_num_1_max_age_vaccine_8",
                     "H_num_1_max_age_vaccine_12",
                     "I_num_2_max_age_vaccine_4",
                     "C_num_2_max_age_vaccine_8",
                     "H_num_2_max_age_vaccine_12",
                     "p_I_num_1_max_age_vaccine_4",
                     "p_C_num_1_max_age_vaccine_8",
                     "p_H_num_1_max_age_vaccine_12",
                     "p_I_num_2_max_age_vaccine_4",
                     "p_C_num_2_max_age_vaccine_8",
                     "p_H_num_2_max_age_vaccine_12")
   
              
# load data ------------------------------------------------------------------- 



national_borders <- st_read(dsn = file.path("output", "shapefiles"),
                            layer = "gadm28_adm0_eras",
                            stringsAsFactors = FALSE)

adm_shp <- st_read(dsn = file.path("output", "shapefiles"), 
                   layer = paste0("gadm28_adm1_eras"),
                   stringsAsFactors = FALSE)


# pre processing -------------------------------------------------------------- 


my_col <- colorRamps::matlab.like(100)


# plot ------------------------------------------------------------------------


for (i in seq_along(vars_to_average)){
  
  var_to_average <- vars_to_average[i]
  
  message(var_to_average)  
  
  if (substr(var_to_average, 1, 1) == "p"){
    
    z_values <- seq(0, 100, 20)
    
  } else {
    
    z_values <- seq(2, 18, 2)  
    
  }
  
  mean_pred_fl_nm <- paste0(var_to_average, "_mean", ".rds")
  
  df_long <- readRDS(file.path(in_path, mean_pred_fl_nm))
  
  out_fl_nm <- paste0(var_to_average, "_", statistic, ".png")
  
  adm_shp_pred <- merge(adm_shp, 
                        df_long[, c("ID_0", "ID_1", statistic)], 
                        by = c("ID_0", "ID_1"), 
                        all.x = TRUE)
  
  quick_polygon_map(adm_shp_pred, my_col, statistic, out_pth, out_fl_nm,
                    z_vals = z_values,
                    country_borders = national_borders)
  
}
