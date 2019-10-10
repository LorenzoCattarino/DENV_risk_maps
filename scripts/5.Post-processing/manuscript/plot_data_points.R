# Creates a map of the data points

library(sf) 
library(ggplot2)


# define parameters -----------------------------------------------------------


parameters <- list(barwdt = 1.5,
                   barhgt = 4.5,
                   pol_brd_sz = 0.1,
                   leg_pos_x = 0,
                   leg_pos_y = 0,
                   leg_txt_sz = 10,
                   leg_ttl_sz = 12,
                   coord_limits = c(-130, 180, -60, 38),
                   adm1_i_want = c("BRA", "COL", "VEN", "MEX", "IND", "AUS"),
                   poly_fill = "grey80",
                   poly_bd_col = "grey40")


# define variables ------------------------------------------------------------


x1 <- parameters$coord_limits[1]
x2 <- parameters$coord_limits[2]
y1 <- parameters$coord_limits[3]
y2 <- parameters$coord_limits[4]

barwdt <- parameters$barwdt

barhgt <- parameters$barhgt

poly_bd_sz <- parameters$pol_brd_sz

leg_pos_x <- parameters$leg_pos_x

leg_pos_y <- parameters$leg_pos_y
  
leg_txt_sz <- parameters$leg_txt_sz

leg_ttl_sz <- parameters$leg_ttl_sz 

adm1_i_want <- parameters$adm1_i_want

poly_fill <- parameters$poly_fill

poly_bd_col <- parameters$poly_bd_col

my_col <- colorRamps::matlab.like(100)


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


z_vals <- seq(0, 0.06, 0.02)
  
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
             aes(x = longitude, y = latitude, colour = FOI), 
             size = 0.7) +
  scale_colour_gradientn(breaks = z_vals,
                         labels = z_vals,
                         limits = c(min(z_vals), max(All_FOI_estimates_sub$FOI)),
                         colours = my_col, 
                         guide = guide_colourbar(title = "FOI", 
                                                 barwidth = barwdt, 
                                                 barheight = barhgt)) +
  coord_sf(xlim = c(x1, x2), ylim = c(y1, y2), expand = FALSE) +
  theme(panel.background = element_rect(fill = "aliceblue"),
        panel.border = element_rect(fill = NA, colour = "black"),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        legend.justification = c(0, 0),
        legend.position = c(leg_pos_x, leg_pos_y),
        plot.margin = unit(c(0, 0, 0, 0), "cm"),
        legend.text = element_text(size = leg_txt_sz),
        legend.title = element_text(face = "bold", size = leg_ttl_sz),
        legend.box.background = element_rect(fill = "white", colour = "black"),
        legend.box.margin = margin(0.025, 0.025, 0.025, 0.025, unit = "cm"))

png(file.path("figures", "data", "dengue_points.png"), 
    width = 18.4, 
    height = 6, 
    units = "cm", 
    pointsize = 12,
    res = 300)

print(p)

dev.off()
 