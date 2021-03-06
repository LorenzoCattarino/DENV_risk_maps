
source(file.path("R", "create_parameter_list.R"))
source(file.path("R", "plotting", "functions_for_plotting_raster_maps.R"))

library(colorRamps)
library(sf)
library(raster)
library(ggplot2)
library(dplyr)
library(shades)


# define parameters ----------------------------------------------------------- 


extra_prms <- list(id = 2,
                   fl_nms = c("response_mean",
                              "transformed_1_wolbachia_4_mean",
                              "transformed_2_wolbachia_4_mean"),
                   vars_to_plot = c("mean", "sd"),
                   plot_wdt = 16,
                   plot_hgt = 5.5, 
                   barwdt = 1.5,
                   barhgt = 4.5,
                   pol_brd_sz = 0.1,
                   leg_pos_x = 0,
                   leg_pos_y = 0,
                   leg_txt_sz = 10,
                   leg_ttl_sz = 12,
                   coord_limits = c(-130, 180, -60, 38),
                   ID_0_to_remove = c(1, 69, 171, 122, 200, 224, 226, 235, 236, 244, 246))

# my_col <- matlab.like(100)
my_col <- creating_color_palette(100)


# define variables ------------------------------------------------------------  


parameters <- create_parameter_list(extra_params = extra_prms)

fl_nms <- parameters$fl_nms

vars_to_plot <- parameters$vars_to_plot

model_type <- paste0("model_", parameters$id)

gr_size <- parameters$resample_grid_size

out_path <- file.path("figures", 
                      "predictions_world",
                      "bootstrap_models",
                      model_type)

coord_limits <- parameters$coord_limits

ID_0_to_remove <- parameters$ID_0_to_remove

fct_combs <- expand.grid(fl_nms, vars_to_plot, stringsAsFactors = FALSE)

names(fct_combs) <- c("fl_nm", "var_to_plot")
                      
                      
# load data ------------------------------------------------------------------- 


countries <- st_read(dsn = file.path("output", "shapefiles"), 
                     layer = "gadm28_adm0_eras")

endemic_ID_0_ID_1 <- read.csv(file.path("output", 
                                        "datasets", 
                                        "dengue_endemic_ID_0_ID_1.csv"),
                              stringsAsFactors = FALSE)


# pre processing -------------------------------------------------------------- 


res <- (1 / 120) * gr_size

lats <- seq(-90, 90, by = res)
lons <- seq(-180, 180, by = res)

endemic_ID_0_ID_1 <- endemic_ID_0_ID_1[!endemic_ID_0_ID_1$ID_0 %in% ID_0_to_remove,]

countries <- countries[!countries$NAME_ENGLI == "Caspian Sea", ]


# loop ------------------------------------------------------------------------


for (i in seq_len(nrow(fct_combs))) {
  
  fl_nm <- fct_combs[i, "fl_nm"]
  
  var_to_plot <- fct_combs[i, "var_to_plot"]
  
  cat("file name =", fl_nm, "\n")
  cat("variable to plot =", var_to_plot, "\n")
  
  if(var_to_plot == "mean") {
    
    if(fl_nm == "response_mean") {
      
      ttl <- "FOI"
      z_vals <- seq(0, 0.06, 0.02)  
      
    }
    
    if(fl_nm == "transformed_1_wolbachia_4_mean") {
      
      ttl <- expression("R"[0])
      z_vals <- seq(1, 7, 2)  
      
    }
    
    if(fl_nm == "transformed_2_wolbachia_4_mean") {
      
      ttl <- expression("R"[0])
      z_vals <- seq(1, 4, 1)  
      
    }
  
  } else {
  
    if(fl_nm == "response_mean") {
      
      ttl <- "SD"
      z_vals <- seq(0, 0.02, 0.01) 
      
    }
    
    if(fl_nm == "transformed_1_wolbachia_4_mean") {
      
      ttl <- "SD"
      z_vals <- seq(0, 2.5, 0.5)  
      
    }
    
    if(fl_nm == "transformed_2_wolbachia_4_mean") {
      
      ttl <- "SD"
      z_vals <- seq(0, 1, 0.2)  
      
    }
  
  }  

  out_file_name <- sprintf("%s_%s%s", fl_nm, var_to_plot, ".png")
  
  pred <- readRDS(file.path("output",
                            "predictions_world",
                            "bootstrap_models",
                            model_type,
                            paste0(fl_nm, ".rds")))
  
  # remove pixels outside of endemic ID_0 and ID_1 
  pred2 <- dplyr::inner_join(pred, endemic_ID_0_ID_1)
  
  pred_mat <- prediction_df_to_matrix(lats, lons, pred2, var_to_plot)
  
  pred_mat_ls <- list(x = lons,
                      y = lats,
                      z = pred_mat)
  
  pred_r_mat <- raster(pred_mat_ls)
  
  pred_r_spdf <- as(pred_r_mat, "SpatialPixelsDataFrame")
  
  pred_r_df <- as.data.frame(pred_r_spdf)
  
  
  # -----------------------------------------------------------------------------
  
  plot_wdt <- parameters$plot_wdt
  plot_hgt <- parameters$plot_hgt
  barwdt <- parameters$barwdt
  barhgt <- parameters$barhgt
  pol_brd_sz <- parameters$pol_brd_sz
  leg_pos_x <- parameters$leg_pos_x
  leg_pos_y <- parameters$leg_pos_y
  leg_txt_sz <- parameters$leg_txt_sz 
  leg_ttl_sz <- parameters$leg_ttl_sz
  bbox <- parameters$coord_limits
  
  x1 <- bbox[1]
  x2 <- bbox[2]
  y1 <- bbox[3]
  y2 <- bbox[4]
  
  p <- ggplot() +
    geom_sf(data = countries, fill = "grey80", color = NA) +
    geom_tile(data = pred_r_df, aes(x = x, y = y, fill = layer)) +
    scale_fill_gradientn(breaks = z_vals,
                         labels = z_vals,
                         limits = c(min(z_vals), max(z_vals)),
                         colours = my_col, 
                         na.value = "grey80",
                         guide = guide_colourbar(title = ttl, 
                                                 barwidth = barwdt, 
                                                 barheight = barhgt)) +
    coord_sf(xlim = c(x1, x2), ylim = c(y1, y2), expand = FALSE) +
    theme(axis.line = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank(),
          legend.justification = c(0, 0),
          legend.position = c(leg_pos_x, leg_pos_y),
          plot.margin = unit(c(0, 0, 0, 0), "cm"),
          panel.background = element_blank(),
          panel.border = element_blank())
  
  dir.create(out_path, FALSE, TRUE)
  
  png(file.path(out_path, out_file_name),
      width = plot_wdt,
      height = plot_hgt,
      units = "cm",
      pointsize = 12,
      res = 300)
  
  print(p)
  
  dev.off()
  
}
