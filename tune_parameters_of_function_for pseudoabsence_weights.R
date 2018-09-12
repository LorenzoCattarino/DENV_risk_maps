source(file.path("R", "prepare_datasets", "set_pseudo_abs_weights.R"))

foi_data <- read.csv(file.path("output", 
                               "foi", 
                               "All_FOI_estimates_and_predictors_2.csv"),
                     stringsAsFactors = FALSE) 

x <- foi_data[foi_data$type == "pseudoAbsence", "Shape_Area"]

parameters <- list(
  shape_1 = 0.1,
  shape_2 = 10,
  shape_3 = 1.5e6)

y <- get_sat_area_wgts(foi_data, parameters)
  
plot(x,y)

parameters <- list(
  b = 0,
  c = 10,
  d = 1e6)
y_c10b0d1 <- get_sat_area_wgts(foi_data, parameters)
points(x, y_c10b0d1, col = "steelblue3")

parameters <- list(
  b = 0,
  c = 5,
  d = 1e6)
y_c5b0d1 <- get_sat_area_wgts(foi_data, parameters)
points(x, y_c5b0d1, col = "yellow3")

parameters <- list(
  b = 0,
  c = 5,
  d = 3e6)
y_c5b0d1 <- get_sat_area_wgts(foi_data, parameters)
points(x, y_c5b0d1, col = "red")

parameters <- list(
  b = 0,
  c = 5,
  d = 1.6e6)
y_c5b0d1 <- get_sat_area_wgts(foi_data, parameters)
points(x, y_c5b0d1, col = "springgreen1")


# plot for paper --------------------------------------------------------------


png(file.path("figures", "data", "saturating_function.png"),
    width = 20,
    height = 10,
    units = "cm",
    pointsize = 12,
    res = 300)

par(mar = c(4,4,1,1), oma = c(0,0,0,0), xaxs="i", yaxs="i")
plot(x,y, type="n", xlab = expression("Area (" ~ km^2 ~ ")"), ylab = "Weight value", axes = FALSE)
points(x, y)
lines(x[order(x)], y[order(y)],type = "l")
axis(1, at = pretty(x))
axis(2, at = pretty(y), las = 2)

dev.off()
