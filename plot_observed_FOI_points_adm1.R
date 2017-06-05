rm(list=ls())

# load packages
library(maptools) 
library(dplyr)
library(colorRamps)

# load data 
All_FOI_estimates <- read.csv (file.path("data", "foi", "All_FOI_estimates_linear_env_var.csv"), header = TRUE, sep = ",")
world_shp_admin_1_dengue <- readShapePoly(file.path("data", "shapefiles", "gadm28_levels.shp", "gadm28_adm1_dengue.shp"))

# Remove outliers 
All_FOI_estimates <- subset(All_FOI_estimates, country != "French Polynesia" & country != "Haiti")

# Get coordinates 
location_xy <- All_FOI_estimates[, c("longitude", "latitude")]

overlay <- over(SpatialPoints(location_xy), world_shp_admin_1_dengue)

All_FOI_estimates$obj_ID <- overlay$OBJECTID

by_adm1_country <- group_by(All_FOI_estimates, adm1, country)

All_FOI_estimates_agg <- summarise(by_adm1_country, mean_FOI = mean (FOI, na.rm = TRUE), OBJECTID = obj_ID[which.max(obj_ID)])

All_FOI_estimates_agg <- as.data.frame(All_FOI_estimates_agg)

shp_file_with_preds <- merge(world_shp_admin_1_dengue, All_FOI_estimates_agg, by = "OBJECTID", all.x = TRUE)

shp_file_with_preds@data$mean_FOI[is.na(shp_file_with_preds@data$mean_FOI)] <- 0

# debug
#All_FOI_estimates_agg[order(All_FOI_estimates_agg$OBJECTID), ]
#All_FOI_estimates_agg[duplicated(All_FOI_estimates_agg$OBJECTID),]

tiff(file.path("figures", "dengue_dataset", 
               "map_of_observed_FOI_values_adm1.tiff"), 
     width = 16, height = 6, units = "in", 
     pointsize = 12,
     compression = "lzw",
     res = 300)

print(spplot(shp_file_with_preds, "mean_FOI", 
             col = NA,
             scales = list(x = list(draw = TRUE, 
                                    at = seq(-150, 150, 50)), 
                           y = list(draw = TRUE,
                                    at = seq(-60, 60, 20))),
             xlab = "Longitude",
             ylab = "Latitude", 
             main = list(label = "observed FOI", cex = 1.3),
             col.regions = matlab.like(400),
             ylim = c(-80, 80)))
dev.off()
