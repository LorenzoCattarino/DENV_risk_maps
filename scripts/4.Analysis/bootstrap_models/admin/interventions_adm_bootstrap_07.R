# Makes a map of mean

# 1) optimal vaccine impact age
# 2) seroprevalence at optimal vaccine impact age

source(file.path("R", "create_parameter_list.R"))
source(file.path("R", "plotting", "quick_polygon_map.R"))

library(sf)
library(ggplot2)


# define parameters -----------------------------------------------------------  


extra_prms <- list(id = 4,
                   vaccine_id = c(28, 32, 36),
                   R0_scenario = c(1, 2),
                   age = c(TRUE, FALSE),
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

vaccine_ids <- parameters$vaccine_id

R0_scenarios <- parameters$R0_scenario

ages <- parameters$age

              
# load data # -----------------------------------------------------------------


fct_c <- read.csv(file.path("output", 
                            "predictions_world", 
                            "bootstrap_models",
                            model_type,
                            "adm_1",
                            "scenario_table_vaccine.csv"))

national_borders <- st_read(dsn = file.path("output", "shapefiles"),
                            layer = "gadm28_adm0_eras",
                            stringsAsFactors = FALSE)

adm_shp <- st_read(dsn = file.path("output", "shapefiles"), 
                   layer = paste0("gadm28_adm1_eras"),
                   stringsAsFactors = FALSE)


# pre processing -------------------------------------------------------------- 


my_col <- colorRamps::matlab.like(100)


# plot ------------------------------------------------------------------------


for (i in seq_along(vaccine_ids)) {
  
  for(j in seq_along(R0_scenarios)) {
    
    for (k in seq_along(ages)) {
      
      vaccine_id <- vaccine_ids[i]
      R0_scenario <- R0_scenarios[j]
      age <- ages[k]
      
      burden_measure <- toupper(substr(fct_c[fct_c$id == vaccine_id, "burden_measure"], 1, 1))
      
      var_to_average <- sprintf("%s_num_%s_max_age_vaccine_%s", burden_measure, R0_scenario, vaccine_id)
      
      z_values <- seq(2, 18, 2)  
      
      if(!age){
        
        z_values <- seq(0, 100, 20)
        
        var_to_average <- sprintf("%s_%s", "p", var_to_average)
        
      }
      
      message(var_to_average)  
      
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
    
  }
  
}
