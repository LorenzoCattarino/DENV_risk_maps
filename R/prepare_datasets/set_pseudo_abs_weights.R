get_area_scaled_wgts <- function(foi_data, wgt_limits){
  
  x <- foi_data[foi_data$type== "pseudoAbsence", "Shape_Area"]
  
  area_limits <- c(min(x), max(x))
  
  y <- rep(0, length(x))
  
  y[which(x==min(x))] <- wgt_limits[1]
  y[which(x==max(x))] <- wgt_limits[2] 
  
  between_lims_ids <- which(y == 0) 
  between_lims <- y[between_lims_ids]
  
  look_up_t <- cbind(x, y) 
  
  interp_wgts <- vapply(look_up_t[between_lims_ids, "x"], 
                        approx_one,
                        numeric(1),
                        a = area_limits,
                        b = wgt_limits)
  
  look_up_t[between_lims_ids, "y"] <- interp_wgts
  
  look_up_t[,"y"]
  
}

approx_one <- function(i, a, b){
  approx(a, b, xout = i)$y
}

get_sat_area_wgts <- function(foi_data, parms){
  
  b <- parms$b 
  c <- parms$c 
  d <- parms$d 
    
  data_sub <- foi_data[foi_data$type== "pseudoAbsence", ]
  
  b + (c - b) * (1 - (1 / (1 + (data_sub$Shape_Area / d))))
    
}
