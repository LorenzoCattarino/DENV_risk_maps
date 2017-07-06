library(data.table)
library(colorRamps)
library(raster)
library(rgdal)


# ----------------------------------------define parameters 


gr_size <- 20

res <- (1 / 120) * gr_size

lats <- seq(-90, 90, by = res)
lons <- seq(-180, 180, by = res)

x <- file.path(
  "output",
  "predictions",
  "best_model_20km_cw",
  "world_0_1667_deg",
  "pred_0_1667_deg_long.txt")

out_path <- file.path(
  "figures", 
  "predictions",
  "best_model_20km_cw",
  "world_0_1667_deg")

out_file_name <- paste("pred_0_1667_deg", "_0_05_deg.png", sep = "")

#world_CRS <- CRS("+proj=aea +datum=WGS84")

e <- extent(-179.0833, 179.0833, -89.08333, 83.08333)


# ---------------------------------------- load data 


all_preds <- fread(x,
                   header = TRUE, 
                   sep = ",",              
                   fill = TRUE, 
                   data.table = FALSE)

country_shp <- readOGR(
  dsn = file.path("output",
            "shapefiles"),
  layer = "gadm28_adm0_eras")


# ---------------------------------------- preprocess shapefile 


country_shp <- country_shp[!country_shp@data$NAME_ENGLI == "Caspian Sea", ]

#country_shp_prj <- spTransform(country_shp, CRS=world_CRS)

country_shp <- crop(country_shp, e)


# ---------------------------------------- create matrix of foi values 


all_preds$lat.int=floor(all_preds$lat.grid*6+0.5)
all_preds$long.int=floor(all_preds$long.grid*6+0.5)
all_preds$foi=ifelse(all_preds$mean_pred<0.01,0,all_preds$mean_pred)

lats.int=lats*6
lons.int=lons*6

mat <- matrix(0, nrow = length(lons), ncol = length(lats))

i.lat <- findInterval(all_preds$lat.int, lats.int)
i.lon <- findInterval(all_preds$long.int, lons.int)

mat[cbind(i.lon, i.lat)] <- all_preds$foi


# ---------------------------------------- convert matrix to raster and clip it 


mat_ls <- list(x = lons,
               y = lats,
               z = mat)

r_mat <- raster(mat_ls)

#r_mat_prj <- projectRaster(r_mat, crs = proj4string(country_shp))

r_mat_cr <- crop(r_mat, e)

r_mat_2 <- mask(r_mat_cr, country_shp)

png(file.path(out_path, out_file_name),
    width = 7, 
    height = 3,
    units = "in",
    pointsize = 12,
    res = 300)
par(mar=c(0,0,0,0))
plot(r_mat_2, col=matlab.like(10), legend = FALSE)
plot(country_shp, col = NA, add = TRUE, lwd = 0.1)
dev.off()
