filter_and_resample <- function(x, foi_dts, env_var_names, grp_flds, grid_size){
  
  xx <- fread(x,
              header = TRUE, 
              sep = ",",              
              na.strings = c("NA", "-1.#IND", "Peipsi", "Moskva", "IJsselmeer", "Zeeuwse meren"),
              fill = TRUE, 
              data.table = FALSE)
  
  #"Peipsi", "Moskva", "IJsselmeer", "Zeeuwse meren"
  if(is.character(xx$ADM_0)) stop("ADM_0 is a character")
  
  #browser()
  
  aa <- merge(
    xx, 
    foi_dts[, c("ADM_0", "ADM_1")], 
    by = c("ADM_0", "ADM_1"),
    all.x = FALSE, 
    all.y = FALSE)
  
  yy <- grid_up(
    dataset = aa, 
    grid_size = grid_size, 
    rnd_dist = FALSE)

  bb <- subset(yy, population != 0)
  
  cc <- average_up(
    pxl_df = bb, 
    grp_flds = grp_flds, 
    var_names = env_var_names)
  
  dd <- merge(
    cc,
    foi_dts[, c("ADM_0", "ADM_1", "type")],
    by = c("ADM_0", "ADM_1"),
    all.x = FALSE,
    all.y = FALSE)
  
  dd$lat.grid <- dd$lat.grid * grid_size
  
  dd$long.grid <- dd$long.grid * grid_size
  
  dd    

}
