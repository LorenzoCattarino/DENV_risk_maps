rm(list = ls())

library(maptools)
library(colorRamps)
library(grid)
library(lattice)

# load functions 
source(file.path("R", "burden_and_interventions", "map_admin_burden.R"))

run_ids <- c(1,2,3,4)

adm_levels <- 1

exp_id <- 5
  
j_flds <- "OBJECTID"

out_path <- file.path(
  "figures", 
  "dengue_dataset", 
  "burden",
  paste("exp", exp_id, sep = "_"))


# ---------------------------------------- load data


# national border shapefile
country_border_shp_fl <- readShapePoly(file.path("data", 
                                                 "shapefiles", 
                                                 "gadm28_levels.shp", 
                                                 "gadm28_adm0.shp"))


# ---------------------------------------- pre processing


var_names <- c(
  "number_of_infections",
  "number_of_cases",
  "incidence_of_infections",
  "incidence_of_cases")

file_names <- var_names

key_titles <- c(
  "Annual number of infections",
  "Annual number of cases",
  "Incidence of infections (100,000 person year)",
  "Incidence of cases (100,000 person year)")

req_n_col <- 400

map_cols <- list(colorRampPalette(c("green4", "yellow", "red"))(req_n_col), 
                 colorRampPalette(c("green4", "yellow", "red"))(req_n_col),
                 colorRampPalette(c("green4", "yellow", "red"))(req_n_col),
                 colorRampPalette(c("green4", "yellow", "red"))(req_n_col))

legend_breaks <- c(6, 6, 6, 6)

do_logs <- c(TRUE, TRUE, FALSE, FALSE)

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


# dataset of burden measures 
burden_datasets <- lapply(run_ids, function(x){
  readRDS(file.path("output", 
                    "dengue_dataset", 
                    "burden", 
                    paste("exp", exp_id, sep = "_"), 
                    paste0("burden_measures_exp_", exp_id, "_run_", x, ".rds")))})

# finer subdivision border shapefiles 
shape_files <- lapply(adm_levels, function(x){
  readShapePoly(file.path("data", 
                          "shapefiles", 
                          "gadm28_levels.shp", 
                          paste0("gadm28_adm", x, ".shp")))})

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

# loop through admin levels
for (i in seq_along(adm_levels)){
 
  run_id <- run_ids[i]
  
  burden_df <- burden_datasets[[i]]
  
  shp_fl <- shape_files[[i]]
  
  
  # ---------------------------------------- creating and saving plot   

  
  # attach burden measures to shp file
  shp_fl_burden <- merge(
    shp_fl, 
    burden_df,
    by = j_flds, 
    all.x = TRUE)
  
  # find NAs
  na_logic <- apply(as.matrix(shp_fl_burden@data[, var_names]), 1, anyNA)
  
  # fix NAs 
  shp_fl_burden@data[na_logic, var_names] <- rep(0, length(var_names))
  
  # loop through variables to plot 
  for (j in seq_along(var_names)){
    
    a <- var_names[j]
    
    b <- file_names[j]
    
    #fl_nm <- paste0(b, "_run_", run_id, ".png")
    fl_nm <- b
    
    c <- map_cols[[j]]
    
    d <- key_titles[j]
    
    e <- legend_breaks[j]
    
    do_log <- do_logs[j]
    
    # if(do_log){
    #   
    #   original_y <- shp_fl_burden@data[, a]
    #   
    #   # log10 transform y_var
    #   shp_fl_burden@data[, a] <- log10(shp_fl_burden@data[, a] + 1)
    #   
    #   # get the values at which you want colours to change
    #   at_vals <- seq(min(shp_fl_burden@data[, a]), max(shp_fl_burden@data[, a]), length.out = 100)
    #   
    #   ### from here on it is hard coded
    #   
    #   # I want the max legend value to be 8 M (original scale)
    #   my_leg_breaks <- c(0, 500000, 2000000, 4000000, 6000000, 8000000)
    #   
    #   # get the values (log10 scale) you want to label in the legend 
    #   at_labs <- c(my_leg_breaks[my_leg_breaks == 0], 
    #                log10(my_leg_breaks[my_leg_breaks > 0]))
    #   
    #   my_labs <- gsub(" ", "", format(my_leg_breaks, scientific = FALSE),
    #                   fixed = TRUE)
    #   
    #   myColorkey <- list(at = at_vals,
    #                      labels = list(
    #                        labels = my_labs,
    #                        at = at_labs, ## where to print labels
    #                        cex = cex_leg_labs),
    #                      width = key_wd,
    #                      height = key_hg)
    # 
    #   # myColorkey <- list(space = "left",
    #   #                    at = at_vals, ## where the colors change
    #   #                    labels = list(
    #   #                      at = at_labs, ## where to print labels
    #   #                      cex = cex_leg_labs),
    #   #                    width = key_wd,
    #   #                    height = key_hg)
    #   
    # }else{
    #   
    #   # get the values you want to label in the legend 
    #   at_labs <- pretty(shp_fl_burden@data[, a], n = e)
    #   
    #   # get the values at which you want colours to change
    #   at_vals <- seq(min(at_labs), max(at_labs), length.out = 100)
    #   
    #   myColorkey <- list(space = "left",
    #                      at = at_vals, ## where the colors change
    #                      labels = list(
    #                        at = at_labs, ## where to print labels
    #                        cex = cex_leg_labs),
    #                      width = key_wd,
    #                      height = key_hg)
    #   
    # }

    # map_predictions_admin(
    #   adm_shp_fl = shp_fl_burden,
    #   nat_bord_shp_fl_ls = country_border_shp_list,
    #   adm_lv = adm_lvl,
    #   y_var = a,
    #   file_name = fl_nm,
    #   map_colours = c,
    #   my_path = out_path, 
    #   plot_scales = FALSE,
    #   x_lab = xl,
    #   y_lab = yl,
    #   key_title = d,
    #   color_key = myColorkey,
    #   low_y_lim = lyl, 
    #   upp_y_lim = uyl,
    #   plot_width = plt_wd, 
    #   plot_height = plt_hg,
    #   leg_title_size = leg_tl_sz,
    #   color_change_values = at_vals)
    
    # plot variable
    map_burden_admin_scale(
      y_var = a, 
      map_file = fl_nm, 
      map_title = NULL, 
      run_id = run_id, 
      exp_id = exp_id, 
      adm_shp_file = shp_fl_burden, 
      country_border_shp_file = country_border_shp_fl, 
      map_colours = c, 
      output_folder = out_path, 
      do.log = do_log)
    
  }
  
}  

prova <- readRDS(
  file.path("output",
            "dengue_dataset",
            "burden",
            "exp_5",
            "burden_measures_exp_5_run_1.rds"))
