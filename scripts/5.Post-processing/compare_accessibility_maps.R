
library(raster)

source(file.path("R", "utility_functions.R"))

out_pt <- file.path("figures", "env_variables", "1_km")

# old accessibility map
old_access <- raster(file.path("data", 
                               "sam's_predictors", 
                               "accessibility_50k_5km.tif"))

# new accessibility map
new_access <- raster(file.path("data", 
                               "accessibility_to_cities_2015_v1.0",
                               "accessibility_to_cities_2015_v1.0.tif"))

values(old_access) <- log(values(old_access))

p1 <- spplot(old_access, at = seq(0, 11, 1))

save_plot(p1, out_pt, "old_travel_time", 17, 10)

values(new_access) <- log(values(new_access))

p2 <- spplot(new_access, at = seq(0, 11, 1))

save_plot(p2, out_pt, "new_travel_time", 17, 10)
