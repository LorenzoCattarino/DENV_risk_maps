# Makes a pretty map of the square predictions  

library(data.table)
library(ggplot2)
library(colorRamps)
library(raster)
library(rgdal)
library(scales)


# ---------------------------------------- define parameters 


model_tp <- "boot_model_20km_cw"

gr_size <- 20

res <- (1 / 120) * gr_size

lats <- seq(-90, 90, by = res)
lons <- seq(-180, 180, by = res)

x <- file.path(
  "output",
  "predictions",
  model_tp,
  "pred_0_1667_deg_long.rds")

out_path <- file.path(
  "figures", 
  "predictions",
  model_tp)

out_file_name <- "pred_0_1667_deg.png"


# ---------------------------------------- load data 


all_preds <- readRDS(x)

country_shp <- readOGR(dsn = file.path("output", "shapefiles"), layer = "gadm28_adm0_eras")


# ---------------------------------------- remove the Caspian Sea


country_shp <- country_shp[!country_shp@data$NAME_ENGLI == "Caspian Sea", ]


# ---------------------------------------- create matrix of foi values 


all_preds$lat.int=floor(all_preds$lat.grid*6+0.5)
all_preds$long.int=floor(all_preds$long.grid*6+0.5)
all_preds$foi=ifelse(all_preds$mean_pred<0.005,0,all_preds$mean_pred)
#all_preds$foi=all_preds$mean_pred

lats.int=lats*6
lons.int=lons*6

mat <- matrix(0, nrow = length(lons), ncol = length(lats))

i.lat <- findInterval(all_preds$lat.int, lats.int)
i.lon <- findInterval(all_preds$long.int, lons.int)

mat[cbind(i.lon, i.lat)] <- all_preds$foi


# ---------------------------------------- convert matrix to raster object


mat_ls <- list(x = lons,
               y = lats,
               z = mat)

r_mat <- raster(mat_ls)


#----------------------------------------- get raster extent 


my_ext <- matrix(r_mat@extent[], nrow = 2, byrow = TRUE) 


# ---------------------------------------- apply same extent to the shape file 


country_shp@bbox <- my_ext


# ---------------------------------------- mask the raster to the shape file


r_mat_msk <- mask(r_mat, country_shp)


# ---------------------------------------- convert to ggplot-friendly objects 


r_spdf <- as(r_mat_msk, "SpatialPixelsDataFrame")

r_df <- as.data.frame(r_spdf)

shp_fort <- fortify(country_shp)


# ---------------------------------------- plot 


dir.create(out_path, FALSE, TRUE)

p <- ggplot(data = r_df, aes(x = x, y = y)) +
  geom_tile(aes(fill = layer)) +
  scale_fill_gradientn(colours = matlab.like(10), 
                       guide = guide_colourbar(title = "FOI", 
                                               barwidth = dev.size()[1] * 0.15, 
                                               barheight = dev.size()[1] * 0.7)) +
  geom_path(data = shp_fort,
            aes(x = long, y = lat, group = group),
            colour = "gray40",
            size = 0.3) +
  coord_equal() +
  scale_x_continuous(labels = NULL, limits = c(-180, 180), expand = c(0, 0)) +
  scale_y_continuous(labels = NULL, limits = c(-60, 90), expand = c(0, 0)) +
  theme_void() + 
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        plot.margin = unit(c(0, 0, 0, -0.09), "cm"),
        legend.position = c(dev.size()[1] * 0.005, dev.size()[1] * 0.008),
        legend.text = element_text(size = 25),
        legend.title = element_text(face = "bold", size = 30))#,
        #legend.background = element_rect(fill = alpha("white", 0.2), colour = "gray50"),
        #panel.background = element_rect(fill = "#A6CEE3", colour = NA)) # lightblue2

png(file.path(out_path, out_file_name),
    width = 28, # original: 7
    height = 12, # original: 3
    units = "in",
    pointsize = 12,
    res = 300)

print(p)

dev.off()
