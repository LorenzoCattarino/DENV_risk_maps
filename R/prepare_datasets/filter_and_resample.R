filter_and_resample <- function(x, foi_dts, env_var_names, grp_flds, jn_flds, grid_size, resample){
  
  # browser()
  
  cat("tile path =", x, "\n")
  
  tile <- fread(x,
                header = TRUE, 
                sep = ",",              
                na.strings = c("NA", "-1.#IND", "Peipsi", "Moskva", "IJsselmeer", "Zeeuwse meren"),
                fill = TRUE, 
                data.table = FALSE)
  
  #"Peipsi", "Moskva", "IJsselmeer", "Zeeuwse meren"
  if(is.character(tile$ID_0)) stop("ID_0 is a character")
  
  aa <- inner_join(tile, foi_dts[, jn_flds])
  
  if(nrow(aa) > 0) {
    
    cc <- clean_and_average(aa, env_var_names, grid_size, c(jn_flds, grp_flds), resample)
    
    dd <- inner_join(cc, foi_dts[, c(jn_flds, "type")])
    
    # names(dd)[names(dd) == "lat.grid"] <- "latitude"
    
    # names(dd)[names(dd) == "long.grid"] <- "longitude"
    
  } else {
    
    dd <- NULL
  
  }
  
  dd

}
