wrapper_to_subset_tile_predictions <- function(x, foi_dts, grp_flds, out_path){
  
  xx <- fread(x,
              header = TRUE, 
              sep = ",",              
              na.strings = c("NA", "-1.#IND", "Peipsi", "Moskva", "IJsselmeer", "Zeeuwse meren"),
              fill = TRUE, 
              data.table = FALSE)
  
  #browser()
  
  if(is.character(xx$ADM_0)) stop("ADM_0 is a character")
  
  a <- grp_flds[2]
  
  zz <- subset(xx, xx[[a]] != -1)
  
  aa <- inner_join(
    zz, 
    foi_dts[, c("data_id", "ADM_0", "ADM_1")])
  
  bb <- subset(aa, population != 0)

  dir.create(out_path, FALSE, TRUE)
  
  write.table(bb, 
              file.path(out_path, basename(x)), 
              row.names = FALSE,
              sep = ",")

}
