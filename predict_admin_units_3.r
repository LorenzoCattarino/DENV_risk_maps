# load packages 
library(maptools)
library(colorRamps)
library(grid)
library(lattice)

# load functions 
source(file.path("R", "random_forest", "plot_foi_map_admin_level.R"))


# ---------------------------------------- define parameters 


adm_levels <- c(1, 2)

j_flds <- "OBJECTID"

in_path_preds <- file.path(
  "output", 
  "dengue_dataset", 
  "predictions", 
  "admin_level_2")

out_path <- file.path(
  "figures", 
  "dengue_dataset", 
  "predictions",
  "admin_level_2")

xl <- ""
yl <- ""
lyl <- -60
uyl <- 90
plt_wd <- 7
plt_hg <- 3
cex_leg_labs <- 0.9
leg_tl_sz <- 12
key_wd <- 1.2
key_hg <- 0.3


# ---------------------------------------- load data


# national border shapefile
country_border_shp_fl <- readShapePoly(file.path("data", 
                                                 "shapefiles", 
                                                 "gadm28_levels.shp", 
                                                 "gadm28_adm0.shp"))


# ---------------------------------------- pre processing


var_names <- c("mean_pred", "sd_pred")

file_names <- c("foi", "sd_foi")

key_titles <- c("FOI", "sd FOI")

map_cols <- list(col_1 = matlab.like(400), 
                 col_2 = rev(heat.colors(400)))

legend_breaks <- c(6, 4)

# Remove Antarctica
country_border_shp_fl <- country_border_shp_fl[!country_border_shp_fl@data$NAME_ENGLI == "Antarctica", ]

# Create list object for country borders
country_border_shp_list <- list("sp.polygons", 
                                country_border_shp_fl, 
                                lwd = 0.2, 
                                col = "gray30", 
                                first = FALSE)


# ----------------------------------------------------------------------------------- 

#                                 functional part start 

# -----------------------------------------------------------------------------------


# dataset of predictions 
prediction_datasets <- lapply(adm_levels, function(x){
  readRDS(file.path(in_path_preds, paste0("predictions_adm_", x, ".rds")))})

# finer subdivision border shapefiles 
shape_files <- lapply(adm_levels, function(x){
  readShapePoly(file.path("data", 
                          "shapefiles", 
                          "gadm28_levels.shp", 
                          paste0("gadm28_adm", x, ".shp")))})
  
# loop through admin levels
for (i in seq_along(adm_levels)){
  
  adm_lvl <- adm_levels[i]
  
  preds_df <- prediction_datasets[[i]]
  
  shp_fls <- shape_files[[i]]

  
  # ---------------------------------------- creating and saving plot
  
  
  # attach mean predictions to shp file
  shp_fl_preds <- merge(shp_fls, preds_df, by = j_flds, all.x = TRUE)
  
  # find NAs
  na_logic <- apply(as.matrix(shp_fl_preds@data[, var_names]), 1, anyNA)
  
  # fix NAs 
  shp_fl_preds@data[na_logic, var_names] <- c(0, 0)
  
  # loop through variables to plot 
  for (j in seq_along(var_names)){
    
    a <- var_names[j]
    
    b <- file_names[j]
    
    fl_nm <- paste0(b, "_map_", "adm_", adm_lvl, ".png")
    
    c <- map_cols[[j]]
    
    d <- key_titles[j]
    
    e <- legend_breaks[j]
      
    # get the values you want to label in the legend 
    at_labs <- pretty(shp_fl_preds@data[, a], n = e)
    
    # get the values at which you want colours to change
    at_vals <- seq(min(at_labs), max(at_labs), length.out = 100)
    # length.out controls the number of times the colour in the legend changes
    # the higher length.out, the more continous is the legend look
    
    myColorkey <- list(space = "left",
                       at = at_vals, ## where the colors change
                       labels = list(
                         at = at_labs, ## where to print labels
                         cex = cex_leg_labs),
                       width = key_wd,
                       height = key_hg)
    
    # plot variable
    map_predictions_admin(
      adm_shp_fl = shp_fl_preds,
      nat_bord_shp_fl_ls = country_border_shp_list,
      adm_lv = adm_lvl,
      y_var = a,
      file_name = fl_nm,
      map_colours = c,
      my_path = out_path, 
      plot_scales = FALSE,
      x_lab = xl,
      y_lab = yl,
      key_title = d,
      color_key = myColorkey,
      low_y_lim = lyl, 
      upp_y_lim = uyl,
      plot_width = plt_wd, 
      plot_height = plt_hg,
      leg_title_size = leg_tl_sz,
      color_change_values = at_vals)
    
  }

}
