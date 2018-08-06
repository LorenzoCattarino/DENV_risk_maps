# Map overlay to find the admin unit of each data point 

library(rgdal)

source(file.path("R", "prepare_datasets", "geolocating_data_points.R"))
source(file.path("R", "utility_functions.R"))


# define parameters -----------------------------------------------------------  


foi_out_pt <- file.path("output", "foi")

foi_out_nm <- "FOI_estimates_lon_lat_gadm.csv"


# load data -------------------------------------------------------------------


FOI_estimates <- read.csv(file.path("output",
                                    "foi",
                                    "FOI_estimates_lon_lat.csv"),
                          header = TRUE,
                          stringsAsFactors = FALSE)

adm_shp <- readOGR(dsn = file.path("output", "shapefiles"), 
                   layer = paste0("gadm28_adm1_eras"),
                   stringsAsFactors = FALSE,
                   integer64 = "allow.loss")


# pre processing --------------------------------------------------------------


out <- matrix(0, nrow = nrow(FOI_estimates), ncol = 2)
  

# loop ------------------------------------------------------------------------


overlay <- map_overlay(FOI_estimates, adm_shp)

FOI_estimates_2 <- cbind(FOI_estimates, ID_0 = overlay$ID_0, ID_1 = overlay$ID_1)

FOI_estimates_2 <- subset(FOI_estimates_2, !is.na(FOI_estimates_2$ID_0))


# save ------------------------------------------------------------------------


write.csv(FOI_estimates_2, 
          file.path(foi_out_pt, foi_out_nm), 
          row.names = FALSE)
