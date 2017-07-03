library(data.table)
library(ggplot2)
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

world_CRS <- CRS("+init=EPSG:4760")


# ---------------------------------------- load data 


all_preds <- fread(x,
                   header = TRUE, 
                   sep = ",",              
                   fill = TRUE, 
                   data.table = FALSE)

country_shp <- readOGR(
  file.path("data",
            "shapefiles",
            "gadm28_levels.shp"),
  "gadm28_adm0")


#shp_fort <- readRDS(file.path("output", "datasets", "country_shp_prj_fort.rds"))


# ---------------------------------------- preprocess shapefile 


country_shp <- country_shp[(country_shp@data$UNREGION1 == "Northern America" | country_shp@data$UNREGION1 == "Central America") & !is.na(country_shp@data$UNREGION1), ]

country_shp <- country_shp[!country_shp@data$NAME_ENGLI == "Antarctica", ]

country_shp_prj <- spTransform(country_shp, world_CRS)


# ---------------------------------------- create matrix of foi values 


all_preds$lat.int=floor(all_preds$lat.grid*6+0.5)
all_preds$long.int=floor(all_preds$long.grid*6+0.5)
all_preds$foi=ifelse(all_preds$mean_pred<0.01,0,all_preds$mean_pred)

lats.int=lats*6
lons.int=lons*6

mat <- matrix(NA, nrow = length(lons), ncol = length(lats))

i.lat <- findInterval(all_preds$lat.int, lats.int)
i.lon <- findInterval(all_preds$long.int, lons.int)

mat[cbind(i.lon, i.lat)] <- all_preds$foi


# ---------------------------------------- convert matrix to raster and clip it 


mat_ls <- list(x = lons,
               y = lats,
               z = mat)

r_mat <- raster(mat_ls, crs = world_CRS)

r_mat_sub <- crop(r_mat, extent(country_shp_prj))

r_mat_sub_2 <- mask(r_mat_sub, country_shp_prj)


# ---------------------------------------- convert to ggplot friendly object 


r_spdf <- as(r_mat_sub_2, "SpatialPixelsDataFrame")

r_df <- as.data.frame(r_spdf)

shp_fort <- fortify(country_shp)


# ---------------------------------------- plot 


# theme_map <- function(...) {
#   theme_void() +
#     theme(axis.line = element_blank(),
#           axis.text.x = element_blank(),
#           axis.text.y = element_blank(),
#           axis.ticks = element_blank(),
#           axis.title.x = element_blank(),
#           axis.title.y = element_blank(),
#           panel.border = element_blank(),
#           panel.spacing = unit(c(0,0,0,0), "cm"),
#           plot.margin = unit(c(0,0,0,0), "cm"),
#           ...
#     )
# }

dir.create(out_path, FALSE, TRUE)

png(file.path(out_path, out_file_name),
    width = 7, 
    height = 3,
    units = "in",
    pointsize = 12,
    res = 300)

p <- ggplot(data = r_df, aes(x = x, y = y)) +
  geom_raster(aes(fill = layer)) +
  coord_equal() +  
  scale_fill_gradientn("FOI", colours = matlab.like(10)) +
  geom_path(data = shp_fort,
            aes(x = long, y = lat, group = group),
            colour = "gray30",
            size = 0.1) +
  labs(x = NULL, y = NULL, title = NULL) +
  theme_void() +
  theme(legend.position = c(0.2, 0.01),
        legend.key.height = unit(0.2, "in"),
        legend.key.width = unit(0.2, "in"))
  # scale_x_continuous(NULL, expand = c(0, 0))+#, limits = range(r_df$x)) +
  # scale_y_continuous(NULL, expand = c(0, 0))#, limits = range(r_df$y))

print(p)

dev.off()
