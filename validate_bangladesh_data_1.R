
# plot the Salje's seroprevalence points
# calculate foi from Salje's seroprevalence values
# find the admin unit 1 ID and centroid for each of the sero points

library(ggplot2)
library(rgdal)
library(dplyr)
library(viridis)
library(rgeos) # for gCentroid()
library(geosphere) # for distm()

source(file.path("R", "prepare_datasets", "calculate_seroprevalence_age.R"))
source(file.path("R", "utility_functions.R"))

# my_h2o_ver <- "3.16.0.2"
# if(packageVersion("h2o") != my_h2o_ver) install.packages(file.path("R_sources", "h2o_3.16.0.2.tar.gz"), repos = NULL, type = "source")
                 
                 
# define parameters -----------------------------------------------------------


alpha_iso_code <- "BGD"

map_out_pt <- file.path("figures", "data", "salje")

dts_out_pt <- file.path("output", "seroprevalence", "salje") 
  
FOI_values <- seq(0, 0.2, by = 0.0002)

dts_out_nm_1 <- "observations_20km.csv"

base_info <- c("reference", 
               "date",
               "country", 
               "ISO",
               "test_location",               
               "longitude",
               "latitude", 
               "no_serotypes",
               "FOI", 
               "variance")

dts_out_nm_2 <- "observations_20km_clean.csv" 


# load data -------------------------------------------------------------------
  
  
salje_data <- read.csv(file.path("data",
                                 "seroprevalence",
                                 "ProportionPositive_bangladesh_salje.csv"))

shp <- readOGR(file.path("data", "shapefiles", "BGD_adm_shp"), "BGD_adm1",
               stringsAsFactors = FALSE,
               integer64 = "allow.loss")

age_distr <- read.csv(file.path("output", 
                                "datasets",
                                "country_age_structure.csv"), 
                      header = TRUE) 


# pre processing --------------------------------------------------------------


shp_fort <- fortify(shp)

salje_data <- cbind(id_point = seq_len(nrow(salje_data)),
                    reference = "Salje", 
                    date = "2014-2016",
                    country = "Bangladesh",
                    ISO = alpha_iso_code,
                    test_location = NA,
                    salje_data,
                    no_serotypes = 4,
                    o_j = salje_data$nPos / salje_data$nAll,
                    variance = 0)


# plot the original seroprevalence points -------------------------------------


dir.create(map_out_pt, FALSE, TRUE)

png(file.path(map_out_pt, "salje_bangl_points_serop.png"),
    width = 12,
    height = 10,
    units = "cm",
    pointsize = 12,
    res = 300)

p <- ggplot() +
  geom_path(data = shp_fort, aes(x = long, y = lat, group = group), size = 0.3) +
  geom_point(data = salje_data, aes(x = longitude, y = latitude, colour = o_j), size = 2) +
  coord_equal() + 
  scale_color_viridis("seroprevalence") +
  theme_minimal()

print(p)

dev.off()

# old style!
#
# par(mar = c(0,0,0,0), oma = c(0,0,0,0))
# plot(shp)
# points(xy_spdf, pch = 21, cex = 0.6, bg = "red")
# text(location_xy$lon, 
#      location_xy$lat, 
#      labels = salje_data$id_point, 
#      pos = 3, 
#      cex = 0.5)


# from seroprevalence to FOI --------------------------------------------------


age_distr <- age_distr[setdiff(names(age_distr), c("band_80_99", "band_85_99"))]

age_band_tgs <- grep("band", names(age_distr), value = TRUE)

age_bounds_num <- sub("^[^_]+_", "", age_band_tgs)

age_bounds_num_2 <- sub("_", "-",age_bounds_num)

names(age_distr)[names(age_distr) %in% age_band_tgs] <- age_bounds_num_2

xx <- strsplit(age_bounds_num_2, "-")
zz <- lapply(xx, as.numeric)
yy <- vapply(zz, mean, numeric(1))

pred_serop <- t(vapply(FOI_values, get_sero, numeric(length(yy)), yy))

BGD_age_struct <- as.matrix(age_distr[age_distr$country == "Bangladesh", age_bounds_num_2])
BGD_age_structure_all_points <- matrix(rep(BGD_age_struct, length(FOI_values)), ncol = 20, byrow = TRUE)

mean_pred_serop <- rowSums(pred_serop * BGD_age_structure_all_points) 

look_up <- data.frame(x = FOI_values, y = mean_pred_serop)

henrik_sero <- salje_data$o_j

henrik_foi <- approx(look_up[, "y"], look_up[, "x"], xout = henrik_sero)$y

salje_data$FOI <- henrik_foi 


# plot the FOI of the original seroprevalence points --------------------------


dir.create(map_out_pt, FALSE, TRUE)

png(file.path(map_out_pt, "salje_bangl_points_FOI.png"),
    width = 12,
    height = 10,
    units = "cm",
    pointsize = 12,
    res = 300)

p <- ggplot() +
  geom_path(data = shp_fort, aes(x = long, y = lat, group = group), size = 0.3) +
  geom_point(data = salje_data, aes(x = longitude, y = latitude, colour = FOI), size = 2) +
  coord_equal() + 
  scale_color_viridis("FOI") +
  theme_minimal()

print(p)

dev.off()


# save ------------------------------------------------------------------------


write.csv(salje_data,
          file.path(dts_out_pt, dts_out_nm_1),
          row.names = FALSE)
