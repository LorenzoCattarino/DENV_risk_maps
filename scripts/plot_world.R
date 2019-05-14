# makes pretty projected map of the world with admin unit 0 boundaries

# load packages
library(rgdal) 
library(ggplot2)


# define parameters -----------------------------------------------------------


poly_fill <- "gray90"

poly_brd_sz <- 0.1

map_proj <- "+proj=moll"


# load data ------------------------------------------------------------------- 


countries <- readOGR(dsn = file.path("output", "shapefiles"), 
                     layer = "gadm28_adm0_eras")

bbox <- readOGR(dsn = file.path("data", "shapefiles", "ne_50m_graticules_all"), 
                layer = "ne_50m_wgs84_bounding_box") 


# pre processing -------------------------------------------------------------- 


countries <- spTransform(countries, CRS(map_proj))

countries_df <- fortify(countries)

bbox <- spTransform(bbox, CRS(map_proj))

bbox_df <- fortify(bbox)


# plot ------------------------------------------------------------------------ 


png(file.path("figures", "world_figure.png"), 
    width = 16.5, 
    height = 8, 
    units = "cm", 
    pointsize = 12,
    res = 100)

p <- ggplot() +
  geom_polygon(data = bbox_df, 
               aes(long, lat, group = group), 
               fill = "aliceblue") +
  geom_polygon(data = countries_df,
               aes(x = long, y = lat, group = group),
               fill = "grey70") +
  geom_path(data = countries_df,
            aes(x = long, y = lat, group = group),
            colour = "gray40",
            size = poly_brd_sz) +
  geom_path(data = bbox_df,
            aes(long, lat, group = group),
            colour = "black",
            size = 0.3) +
  coord_equal() +
  theme_void() +
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))

print(p)

dev.off()
