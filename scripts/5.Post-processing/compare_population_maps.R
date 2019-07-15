
library(raster)
library(rgdal)
library(countrycode)
library(gridExtra)
library(viridis)
library(ggplot2)

source(file.path("R", "utility_functions.R"))

out_pt <- file.path("figures", "env_variables", "1_km")

landscan_pop <- raster(file.path("data", 
                                 "Landscan_2015", 
                                 "lspop2015.flt")) 

worldpop_pop <- raster(file.path("data",
                                 "Worldpop_Africa_2000_2020", 
                                 "AFR_PPP_2015_adj_v2.tif")) 

adm_shp <- readOGR(dsn = file.path("output", "shapefiles"), 
                   layer = paste0("gadm28_adm1_eras"))

adm_shp@data$continent <- countrycode(sourcevar = adm_shp@data$NAME_0, 
                                      origin = "country.name", 
                                      destination = "continent")

adm_shp_sub <- subset(adm_shp, continent == "Africa")

ext_worldp <- extent(worldpop_pop)

ext_worldp[1] <- -22
ext_worldp[2] <- 54
ext_worldp[3] <- -18
ext_worldp[4] <- 23
  
landscan_pop_cr <- crop(landscan_pop, ext_worldp)
worldpop_pop_cr <- crop(worldpop_pop, ext_worldp)

values(worldpop_pop_cr) <- log(values(worldpop_pop_cr))
values(landscan_pop_cr) <- log(values(landscan_pop_cr))

my_cols <- rev(viridis(50))

country_border_shp_list <- list("sp.polygons", 
                                adm_shp_sub, 
                                lwd = 1, 
                                col = "gray40", 
                                first = FALSE)

p1 <- spplot(worldpop_pop_cr, 
             main = list(label = "WorldPop", cex = 1),
             at = seq(0, 11, 1), 
             col.regions = my_cols,
             sp.layout = list(country_border_shp_list))

save_plot(p1, out_pt, "africa_worldpop", 17, 10)

p2 <- spplot(landscan_pop_cr, 
             main = list(label = "LandScan", cex = 1),
             at = seq(0, 11, 1),
             col.regions = my_cols,
             sp.layout = list(country_border_shp_list))

save_plot(p2, out_pt, "africa_landscan", 17, 10)

p3 <- arrangeGrob(p1, p2, nrow = 2)

ggsave(file.path(out_pt, "africa_landscan_worldpop.png"),
       p3)
