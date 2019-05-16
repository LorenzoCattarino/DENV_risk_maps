library(sf)
library(lwgeom) # st_make_valid()

out_dir <- file.path("figures", "sf")

lakes <- read_sf(dsn = file.path("output", "shapefiles"),
                 layer = "lakes_diss")

# adm0_erase_arcGIS <- readOGR(dsn = file.path("data", "shapefiles", "gadm28_levels.shp"), 
#                              layer = "gadm28_adm0_eras")

countries <- read_sf(dsn = file.path("data", "shapefiles", "gadm28_levels.shp"), 
                     layer = "gadm28_adm0")

# subset ----------------------------------------------------------------------


canada <- countries[countries$NAME_ENGLI == "Canada", ]

lakes_val <- st_make_valid(lakes)

lakes_val_sub <- lakes_val[lakes_val$Name1 == "Lake Michigan" | lakes_val$Name1 == "Lake Superior", ]

canada_no_lakes <- st_difference(canada, lakes_val_sub)


# plot ------------------------------------------------------------------------


shp_fl <- st_geometry(canada_no_lakes)

dir.create(out_dir, FALSE, TRUE)

png(file.path(out_dir, "canada_no_lakes.png"),
    width = 16,
    height = 8,
    units = "cm",
    pointsize = 12,
    res = 300)
par(mar = c(0.5,0.5,0.5,0.5), oma = c(0,0,0,0))
plot(shp_fl, col = "red", border = "green", lwd = 0.5)
# plot(st_geometry(lakes), col = "blue", border = "black", lwd = 0.5, add = TRUE)
dev.off()
