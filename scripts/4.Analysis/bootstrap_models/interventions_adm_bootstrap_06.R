# Makes a map of the mean max vaccine impact age

source(file.path("R", "create_parameter_list.R"))
source(file.path("R", "plotting", "quick_polygon_map.R"))

library(rgdal)
library(lattice)


# define parameters -----------------------------------------------------------  


extra_prms <- list(id = 4,
                   vaccine_id = 4,
                   R0_scenario = 1,
                   statistic = "mean")


# define variables ------------------------------------------------------------


parameters <- create_parameter_list(extra_params = extra_prms)

model_type <- paste0("model_", parameters$id)

R0_scenario <- parameters$R0_scenario

vaccine_id <- parameters$vaccine_id

statistic <- parameters$statistic

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


# load data ------------------------------------------------------------------- 


fct_c <- read.csv(file.path("output", 
                            "predictions_world", 
                            "bootstrap_models",
                            model_type,
                            "adm_1",
                            "scenario_table_vaccine.csv"))

adm_shp <- readOGR(dsn = file.path("output", "shapefiles"), 
                   layer = paste0("gadm28_adm1_eras"),
                   stringsAsFactors = FALSE)


# pre processing -------------------------------------------------------------- 


my_col <- colorRamps::matlab.like(100)

adm_shp_2 <- adm_shp[!duplicated(data.frame(adm_shp)[,c("ID_0", "ID_1")]),]

burden_measure <- toupper(substr(fct_c[vaccine_id, "burden_measure"], 1, 1))

vars_to_average <- sprintf("%s_num_%s_max_age_vaccine_%s", burden_measure, R0_scenario, vaccine_id)

mean_pred_fl_nm <- paste0(vars_to_average, "_mean", ".rds")

df_long <- readRDS(file.path(in_path, mean_pred_fl_nm))

out_fl_nm <- paste0(vars_to_average, "_", statistic, ".png")

adm_shp_pred <- merge(adm_shp_2, 
                      df_long[, c("ID_0", "ID_1", statistic)], 
                      by = c("ID_0", "ID_1"), 
                      all.x = TRUE)

# fill NA 
# adm_shp_pred@data[is.na(adm_shp_pred@data[, statistic]), statistic] <- 0


# plot ------------------------------------------------------------------------


quick_polygon_map(adm_shp_pred, my_col, statistic, out_pth, out_fl_nm)
