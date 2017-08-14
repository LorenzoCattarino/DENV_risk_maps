filter_and_resample <- function(x, foi_dts, env_var_names, grp_flds, grid_size){
  
  #browser()
  
  xx <- fread(x,
              header = TRUE, 
              sep = ",",              
              na.strings = c("NA", "-1.#IND", "Peipsi", "Moskva", "IJsselmeer", "Zeeuwse meren"),
              fill = TRUE, 
              data.table = FALSE)
  
  #"Peipsi", "Moskva", "IJsselmeer", "Zeeuwse meren"
  if(is.character(xx$ADM_0)) stop("ADM_0 is a character")
  
  aa <- inner_join(
    xx, 
    foi_dts[, c("unique_id", "data_id", "ADM_0", "ADM_1")])
  
  yy <- grid_up(
    dataset = aa, 
    grid_size = grid_size, 
    rnd_dist = FALSE)

  bb <- subset(yy, population != 0)
  
  cc <- average_up(
    pxl_df = bb, 
    grp_flds = grp_flds, 
    var_names = env_var_names)
  
  dd <- inner_join(
    cc,
    foi_dts[, c("unique_id", "data_id", "ADM_0", "ADM_1", "type", "adm_pop")])
  
  dd$lat.grid <- dd$lat.grid * grid_size
  
  dd$long.grid <- dd$long.grid * grid_size
  
  names(dd)[names(dd) == "lat.grid"] <- "latitude"
  
  names(dd)[names(dd) == "long.grid"] <- "longitude"
  
  dd    

}
