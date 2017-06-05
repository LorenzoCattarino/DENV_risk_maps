rm(list = ls())

library(maptools)
library(colorRamps)
library(grid)
library(lattice)
library(RColorBrewer)

# load functions 
source(file.path("R", "plot_map_pixel_level.R"))

input_path <- file.path("output", "dengue_dataset", "env_variables", "world_0_05_deg")

input_file_names <- list.files(input_path, 
                               pattern = ".*.txt",
                               full.names = FALSE)

out_path <- file.path("figures", "env_variables", "0_05_deg")


# ----------------------------------------load data


input_data <- lapply(input_file_names, 
                     function(x){ 
                       as.matrix(read.table(
                         file.path(input_path, x), 
                         sep = ","))})

country_border_shp_fl <- readShapePoly(
  file.path("data",
            "shapefiles",
            "gadm28_levels.shp",
            "gadm28_adm0.shp"))


# ----------------------------------------pre processing 


file_names <- sub("\\..*", "", input_file_names)

output_file_names <- paste(file_names, "_0_05_deg.png", sep = "")

# remove Antarctica
country_brds <- country_border_shp_fl[!country_border_shp_fl@data$NAME_ENGLI == "Antarctica", ]

# define key titles 
key_ttls <- c(
  "Altitude", 
  "Amplitude (D temp)", 
  "Diurnal temperature", 
  "Amplitude (EVI)",
  "EVI",
  "Amplitude (MIR)",
  "MIR",
  "Amplitude (N temp)",
  "Nocturnal temperature",
  "Amplitude (Precip)", 
  "Precipitation")

#display.brewer.all()

n_pal_col <- 11 
req_n_col <- 400 
amp_palette <- colorRampPalette(brewer.pal(n_pal_col, "OrRd"))(req_n_col)

alt_p <- terrain.colors(req_n_col)
daytemp_p <- rev(colorRampPalette(brewer.pal(n_pal_col, "RdBu"))(req_n_col)) 
daytemp_amp_p <- amp_palette
EVI_p <- colorRampPalette(brewer.pal(n_pal_col, "Greens"))(req_n_col)  
EVI_amp_p <- amp_palette 
MIR_p <- colorRampPalette(brewer.pal(n_pal_col, "Spectral"))(req_n_col) 
MIR_amp_p <- amp_palette
nighttemp_p <- rev(colorRampPalette(brewer.pal(n_pal_col, "RdYlBu"))(req_n_col))
nighttemp_amp_p <- amp_palette
#RFE_p <- colorRampPalette(brewer.pal(n_pal_col, "Blues"))(req_n_col)
RFE_p <- topo.colors(req_n_col)
RFE_amp_p <- amp_palette

palettes <- list(alt_p,
                 daytemp_amp_p,
                 daytemp_p,
                 EVI_amp_p,
                 EVI_p,
                 MIR_amp_p,
                 MIR_p,
                 nighttemp_amp_p,
                 nighttemp_p,
                 RFE_amp_p,
                 RFE_p)

col_breaks <- c(4, 4, 4, 5, 4, 6, 5, 6, 6, 5, 6)

key_ttl_xs <- c(0.18, 0.13, 0.13, 0.15, 0.18, 0.15, 0.18, 0.13, 0.12, 0.14, 0.16) 

res <- 1/20
lats <- seq(-90+res, 90, by = res) # dodgy but effective, as length(row.values) == nrow(x)  in levelplot must be TRUE
lons <- seq(-180+res, 180, by = res)

plt_wd <- 7
plt_hg <- 3
row_vls <- lons
col_vls <- lats
xl <- ""
yl <- ""
lyl <- -60
uyl <- 90
cex_leg_labs <- 0.9
leg_tl_sz <- 12
key_wd <- 1.2
key_hg <- 0.3

for (i in seq_along(input_data)){
  
  ktx <- key_ttl_xs[i]
  
  out_fl_nm <- output_file_names[i]
  
  my_k_ttl <- key_ttls[[i]]
  
  n_brk <- col_breaks[[i]]
  
  my_col <- palettes[[i]]
  
  my_mat <- input_data[[i]]
  
  nr <- nrow(my_mat)
  
  my_mat_2 <- t(my_mat[nr:1,])
  
  at_labs <- pretty(my_mat_2, n = n_brk)
  
  # get the values at which you want colours to change
  at_vals <- seq(min(at_labs), max(at_labs), length.out = 100)
  
  myColorkey <- list(space = "left",
                     at = at_vals, ## where the colors change
                     labels = list(
                       at = at_labs, ## where to print labels
                       cex = cex_leg_labs),
                     width = key_wd,
                     height = key_hg)
  
  # plot variable
  map_data_pixel(
    data_to_plot = my_mat_2, 
    country_borders = country_brds, 
    output_path = out_path, 
    file_name = out_fl_nm, 
    plot_width = plt_wd, 
    plot_height = plt_hg,
    row_values = row_vls, 
    col_values = col_vls, 
    color_change_values = at_vals,
    x_lab = xl, 
    y_lab = yl, 
    key_title = my_k_ttl,
    color_key = myColorkey,
    low_y_lim = lyl, 
    upp_y_lim = uyl, 
    map_colours = my_col, 
    leg_title_size = leg_tl_sz,
    key_ttl_x = ktx)  
  
}
