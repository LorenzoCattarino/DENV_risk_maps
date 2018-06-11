# Creates a map of the dataset point and dengue presence-absence mask 

# load packages
library(rgdal) 
library(ggplot2)


# define parameters -----------------------------------------------------------


adm1_i_want <- c("BRA", "COL", "VEN", "MEX", "IND", "AUS")

poly_fill <- "gray90"

poly_bd_sz <- 0.1


# load data ------------------------------------------------------------------- 


All_FOI_estimates <- read.table(file.path("output", 
                                          "foi", 
                                          "All_FOI_estimates_linear.txt"), 
                                header = TRUE, 
                                sep = ",")

countries <- readOGR(dsn = file.path("output", "shapefiles"), 
                     layer = "gadm28_adm0_eras")

adm1 <- readOGR(dsn = file.path("output", "shapefiles"), 
                layer = "gadm28_adm1_eras")


# pre processing -------------------------------------------------------------- 


adm1_sub <- subset(adm1, ISO %in% adm1_i_want)

countries_fort <- fortify(countries)

adm1_sub_fort <- fortify(adm1_sub)


# plot ------------------------------------------------------------------------ 


png(file.path("figures", "data", "dengue_points.png"), 
    width = 16.5, 
    height = 8, 
    units = "cm", 
    pointsize = 12,
    res = 300)

p <- ggplot() +
  geom_polygon(data = countries_fort,
               aes(x = long, y = lat, group = group),
               colour = "black",
               fill = poly_fill,
               size = poly_bd_sz) +
  geom_path(data = adm1_sub_fort,
            aes(x = long, y = lat, group = group),
            colour = "black",
            size = poly_bd_sz) +
  geom_point(data = All_FOI_estimates, 
             aes(x = longitude, y = latitude), 
             size = 0.5,
             colour = "blue") +
  coord_equal() + # does not change underlying data
  theme_void() +
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))

print(p)

dev.off()
  