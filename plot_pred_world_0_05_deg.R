library(maptools)
library(colorRamps)
library(grid)
library(lattice)
library(data.table)

# load functions 
source(file.path("R", "prepare_datasets", "plot_map_pixel_level.R"))


# ---------------------------------------- define parameters


input_path <- file.path(
  "output", 
  "predictions", 
  "best_model_30km_cw",
  "world_0_05_deg")

input_file_names <- list.files(input_path, 
                               pattern = "wide.*.txt",
                               full.names = FALSE)

out_path <- file.path(
  "figures", 
  "predictions",
  "best_model_30km_cw")

key_ttls <- c("FOI", "sd FOI")

req_n_col <- 400 
foi_p <- matlab.like(req_n_col)
sd_foi_p <- rev(heat.colors(req_n_col))  

palettes <- list(foi_p,
                 sd_foi_p)

col_breaks <- c(6, 4)

key_ttl_xs <- c(0.18, 0.18)

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


# ----------------------------------------load data


input_data <- lapply(input_file_names, 
                     function(x){ 
                       as.matrix(fread(
                         file.path(input_path, x), 
                         header = TRUE, 
                         sep = ",",              
                         fill = TRUE, 
                         data.table = FALSE))})

country_border_shp_fl <- rgdal::readOGR(
  file.path("data",
            "shapefiles",
            "gadm28_levels.shp"),
            "gadm28_adm0")


# ----------------------------------------pre processing 


file_names <- sub("\\..*", "", input_file_names)

output_file_names <- paste(file_names, "_0_05_deg.png", sep = "")

# remove Antarctica
country_brds <- country_border_shp_fl[!country_border_shp_fl@data$NAME_ENGLI == "Antarctica", ]


# ---------------------------------------- start


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
  
  max_at_val <- max(at_labs)
  #max_at_val <- 0.05
  
  # get the values at which you want colours to change
  at_vals <- seq(min(at_labs), max_at_val, length.out = 100)
  
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
