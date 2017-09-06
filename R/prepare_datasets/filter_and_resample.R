filter_and_resample <- function(x, foi_dts, env_var_names, grp_flds, grid_size){
  
  #browser()
  
  cat("tile path =", x, "\n")
  
  tile <- fread(x,
              header = TRUE, 
              sep = ",",              
              na.strings = c("NA", "-1.#IND", "Peipsi", "Moskva", "IJsselmeer", "Zeeuwse meren"),
              fill = TRUE, 
              data.table = FALSE)
 
  #"Peipsi", "Moskva", "IJsselmeer", "Zeeuwse meren"
  if(is.character(tile$ADM_0)) stop("ADM_0 is a character")
  
  aa <- inner_join(
    tile, 
    foi_dts[, c("unique_id", "data_id", "ADM_0", "ADM_1")])
  
  if(nrow(aa) > 0) {
    
    cc <- clean_and_resample(aa, env_var_names, grid_size, grp_flds)
    
    dd <- inner_join(
      cc,
      foi_dts[, c("unique_id", "data_id", "ADM_0", "ADM_1", "type", "adm_pop")])
    
    names(dd)[names(dd) == "lat.grid"] <- "latitude"
    
    names(dd)[names(dd) == "long.grid"] <- "longitude"
    
  } else {
    
    dd <- NULL
  
  }
  
  dd

}
