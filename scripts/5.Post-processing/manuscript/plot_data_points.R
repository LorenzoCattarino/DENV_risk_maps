# Creates a map of the data points

library(sf) 
library(ggplot2)


# define parameters -----------------------------------------------------------


parameters <- list(coord_limits = c(-130, 180, -60, 38),
                   adm1_i_want = c("BRA", "COL", "VEN", "MEX", "IND", "AUS"),
                   poly_fill = "grey80",
                   poly_bd_sz = 0.1,
                   poly_bd_col = "grey40")
  

# define variables ------------------------------------------------------------


x1 <- parameters$coord_limits[1]
x2 <- parameters$coord_limits[2]
y1 <- parameters$coord_limits[3]
y2 <- parameters$coord_limits[4]

adm1_i_want <- parameters$adm1_i_want
  
poly_fill <- parameters$poly_fill

poly_bd_sz <- parameters$poly_bd_sz

poly_bd_col <- parameters$poly_bd_col


# load data ------------------------------------------------------------------- 


All_FOI_estimates <- read.csv(file.path("output", 
                                        "foi", 
                                        "All_FOI_estimates_and_predictors.csv"),
                              stringsAsFactors = FALSE) 

countries <- read_sf(dsn = file.path("output", "shapefiles"), 
                     layer = "gadm28_adm0_eras")

adm1 <- read_sf(dsn = file.path("output", "shapefiles"), 
                layer = "gadm28_adm1_eras")


# pre processing -------------------------------------------------------------- 


All_FOI_estimates_sub <- subset(All_FOI_estimates, type != "pseudoAbsence")

adm1_sub <- subset(adm1, ISO %in% adm1_i_want)


# plot ------------------------------------------------------------------------ 


png(file.path("figures", "data", "dengue_points.png"), 
    width = 17, 
    height = 5.5, 
    units = "cm", 
    pointsize = 12,
    res = 300)

p <- ggplot() +
  geom_sf(data = countries, 
          fill = poly_fill, 
          colour = poly_bd_col, 
          size = poly_bd_sz) +
  geom_sf(data = adm1_sub, 
          fill = NA, 
          colour = poly_bd_col, 
          size = poly_bd_sz) +
  geom_point(data = All_FOI_estimates_sub, 
             aes(x = longitude, y = latitude), 
             size = 0.5,
             colour = "blue") +
  coord_sf(xlim = c(x1, x2), ylim = c(y1, y2), expand = FALSE) +
  theme(legend.position = "none",
        plot.margin = unit(c(0, 0, 0, 0), "cm"),
        axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_blank())

print(p)

dev.off()
 