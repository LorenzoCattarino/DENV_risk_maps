# Map overlay to find the admin unit of each data point 


library(rgdal)

source(file.path("R", "prepare_datasets", "geolocating_data_points.R"))
source(file.path("R", "utility_functions.R"))


# define parameters -----------------------------------------------------------  


foi_out_pt <- file.path("output", "foi")

foi_out_nm <- "FOI_estimates_lon_lat_gadm.csv"

records_to_tweak <- c(3, 
                      7,
                      15, 
                      19, 
                      21, 
                      24, 
                      29, 
                      31,
                      33, 
                      36, 
                      38, 
                      42, 
                      44, 
                      45, 
                      46, 
                      91, 
                      93, 
                      92, 
                      113,
                      139,
                      144)

new_x <- c(103.82, 
           -97.464746,
           103.82, 
           -84.09, 
           55.610809, 
           80, 
           100.384533, 
           -97.464746,
           145.7, 
           80, 
           80, 
           -67.4, 
           -69.8372, 
           103.82, 
           103.82, 
           91.460713, 
           91.460713, 
           91.460713,
           89.841560, 
           88.500550,
           91.324555)

new_y <- c(1.49, 
           25.992059,
           1.49, 
           9.8, 
           -21.0514, 
           6.8, 
           14.590194, 
           25.992059,
           -5.222, 
           6.8, 
           6.8, 
           10.15,
           18.47, 
           1.49, 
           1.49, 
           23.042815, 
           23.042815, 
           23.042815,
           23.519344, 
           26.477633,
           24.323034)


# load data -------------------------------------------------------------------


FOI_estimates <- read.csv(file.path("data",
                                    "foi",
                                    "FOI_estimates_lon_lat.csv"),
                          header = TRUE,
                          stringsAsFactors = FALSE)

adm_shp <- readOGR(dsn = file.path("output", "shapefiles"), 
                   layer = paste0("gadm28_adm1_eras"),
                   stringsAsFactors = FALSE,
                   integer64 = "allow.loss")


# pre processing --------------------------------------------------------------


# remove Philippines CaseReport data
FOI_estimates <- subset(FOI_estimates, !(ISO == "PHL" & type == "caseReport")) 


# find adm0 and adm1 codes ----------------------------------------------------


overlay <- map_overlay(FOI_estimates, adm_shp)

FOI_estimates_2 <- cbind(FOI_estimates, ID_0 = overlay$ID_0, ID_1 = overlay$ID_1)


# tweak coordinates -----------------------------------------------------------


FOI_estimates_2$longitude[records_to_tweak] <- new_x
FOI_estimates_2$latitude[records_to_tweak] <- new_y


# check which points have changed adm0 and/or adm1 ----------------------------


overlay_2 <- map_overlay(FOI_estimates_2, adm_shp)

new_adm0_adm1 <- cbind(ID_0 = overlay_2$ID_0, ID_1 = overlay_2$ID_1)

FOI_estimates_2[records_to_tweak, c("ID_0", "ID_1")]
new_adm0_adm1[records_to_tweak, c("ID_0", "ID_1")]


# -----------------------------------------------------------------------------
#
# fix the coordinates of those points 
# or replace adm0 and adm1 codes if unfixable 
# (becasue the 1/6 degree cell is missing - NA covariates)

FOI_estimates_2[records_to_tweak, c("ID_0", "ID_1")] <- new_adm0_adm1[records_to_tweak, c("ID_0", "ID_1")]


# save ------------------------------------------------------------------------


write_out_csv(FOI_estimates_2, foi_out_pt, foi_out_nm)
