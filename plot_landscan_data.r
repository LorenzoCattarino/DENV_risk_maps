# Plot landscan population data - example for Australia

landscan_pop <- raster("data/Landscan_2015/lspop2015.flt")

new.extent <- extent(c(110, 160, -40, -10))

landscan_pop_cropped <- crop(landscan_pop, new.extent)

values(landscan_pop_cropped) <- log(values(landscan_pop_cropped)) 
  
plot(landscan_pop_cropped)
