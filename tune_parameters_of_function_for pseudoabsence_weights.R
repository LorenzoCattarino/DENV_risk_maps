source(file.path("R", "prepare_datasets", "set_pseudo_abs_weights.R"))

foi_data <- read.csv(file.path("output", 
                               "foi", 
                               "All_FOI_estimates_linear_env_var_area_salje.csv"),
                     stringsAsFactors = FALSE) 

x <- foi_data[foi_data$type == "pseudoAbsence", "Shape_Area"]

parameters <- list(
  b = 0.1,
  c = 10,
  d = 1.5e6)

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
