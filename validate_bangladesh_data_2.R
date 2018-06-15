

# define parameters -----------------------------------------------------------


adm0 <- 20

bng_map_out_pt <- file.path("figures", "env_variables", "Bangladesh")


# load data -------------------------------------------------------------------


covariates <- readRDS(file.path("output",
                                "env_variables",
                                "all_squares_env_var_0_1667_deg_dis.rds"))

predictor_rank <- read.csv(file.path("output", 
                                     "variable_selection", 
                                     "metropolis_hastings", 
                                     "exp_1", 
                                     "variable_rank_final_fits_exp_1.csv"),
                           stringsAsFactors = FALSE)


# pre process -----------------------------------------------------------------


names(covariates)[names(covariates) == "longitude"] <- "long.grid"
names(covariates)[names(covariates) == "latitude"] <- "lat.grid"

covariates$pop_density <- covariates$population / 342
covariates$pop_density <- log(1 + covariates$pop_density)


# subset covariate dataset (bangladesh) ---------------------------------------


covariates_bgd <- covariates[covariates$ADM_0 == adm0, ]


# rescale covariate -----------------------------------------------------------


all_predictors <- predictor_rank$name

for (i in seq_along(all_predictors)){
  
  var <- all_predictors[i]
  
  scale <- 1
  
  if(grepl("Re.", var) | grepl("Im.", var)){
    
    scale <- ppyear * (year.f - year.i + 1) / 2 
    
  } 
  
  if(grepl("const_term$", var)){
    
    scale <- ppyear * (year.f - year.i + 1) 
    
  }  
  
  # message(scale)
  
  covariates_bgd[, var] <- covariates_bgd[, var] / scale
  
}


# map covariates --------------------------------------------------------------


all_predictors <- c(all_predictors, "pop_density")

dir.create(bng_map_out_pt, FALSE, TRUE)

for (j in seq_along(all_predictors)){
  
  my_pred <- all_predictors[j]
  
  png(file.path(bng_map_out_pt, paste0(j, "_", my_pred,".png")),
      width = 13,
      height = 10,
      units = "cm",
      pointsize = 12,
      res = 300)
  
  p <- ggplot() +
    geom_tile(data = covariates_bgd, aes_string(x = "long.grid", y = "lat.grid", fill = my_pred)) +
    geom_point(data = salje_data, aes(x = lon, y = lat, colour = o_j), size = 1.5) +
    geom_point(data = our_foi_point, aes(x = longitude, y = latitude), colour = "red", size = 2) +
    scale_fill_gradientn(colours = my_col_cov,
                         guide = guide_colourbar(title = my_pred)) +
    scale_color_viridis("seroprevalence") +
    geom_path(data = shp_fort, aes(x = long, y = lat, group = group), size = 0.3) +
    scale_x_continuous("longitude", limits = c(88, 93)) +
    scale_y_continuous("latitude", limits = c(20, 27), breaks = seq(20, 27, 1), labels = seq(20, 27, 1)) +
    coord_equal() +
    theme_minimal()
  
  print(p)
  
  dev.off()
  
}
