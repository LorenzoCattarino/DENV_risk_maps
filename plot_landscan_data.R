library(raster)
LSpop <- raster("//fi--didef2/Census/Landscan2014/Population/lspop2014.flt")
values(LSpop) <- log(values(LSpop))
plot(LSpop)
