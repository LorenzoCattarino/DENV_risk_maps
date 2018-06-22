rm(list = ls())

# load packages 
library(data.table)
library(maptools)
library(fields)
library(RColorBrewer)

# load data
env_var <- fread(file.path("data", 
                           "env_variables", 
                           "environ0.25_B.txt"))

# convert to df 
env_var_df <- as.data.frame(env_var)

# define parameters
res <- 0.25
lats <- seq(-60, 60, by = res)
lons <- seq(-180, 180, by = res)
year.i <- 2007
year.f <- 2014
ppyear <- 64
ntimes <- 1
mycol <- rev(colorRampPalette(brewer.pal(11, "RdYlBu"))(100))
variables <- c("altitude", "DayTemp_const_term", "NightTemp_const_term", "RFE_const_term", "EVI_const_term", "MIR_const_term")

output_path <- file.path("figures", "env_variables")

dir.create(output_path, FALSE, TRUE)

# loop through the variables to plot 
for (i in seq_along(variables)) {
  
  var <- variables[i]
    
  ## dealing with NA values:
  if(grepl("DayTemp", var) | grepl("NightTemp", var)) {
    na.val <- -273*ntimes
    na.rows <- which(env_var_df[, var] < na.val)
    env_var_df[na.rows, -(1:3)] <- NA
    ntimes <- (year.f - year.i + 1)*ppyear
  }
  
  if(grepl("EVI", var)) {
    na.val <- -0.2*ntimes
    na.rows <- which(env_var_df[, var] < na.val)
    env_var_df[na.rows, -(1:3)] <- NA
    ntimes <- (year.f - year.i + 1)*ppyear
  } 
  
  if(grepl("MIR", var)) {
    na.val <- 0*ntimes
    na.rows <- which(env_var_df[, var] < na.val)
    env_var_df[na.rows, -(1:3)] <- NA
    ntimes <- (year.f - year.i + 1)*ppyear
  }    
  
  mat <- matrix(NA, nrow = length(lons) - 1, ncol = length(lats) - 1)
  i.lat <- findInterval(env_var_df$latitude, lats)
  i.lon <- findInterval(env_var_df$longitude, lons)
  
  png(file.path(output_path, paste0(var, ".png")), width = 7, height = 3, units = "in", res = 300)
  
  par(mai = c(0,0,0.5,0.5), oma = c(0,0,0.5,0))
  
  ## annual mean
  mat[cbind(i.lon, i.lat)] <- env_var_df[, var] / ntimes
  
  image.plot(lons, lats, mat, 
             asp = 1, 
             col = mycol, 
             xlab = "", 
             ylab = "", 
             axes = FALSE,
             legend.shrink = 0.7, 
             legend.width = 1)
  
  dev.off()
  
}
