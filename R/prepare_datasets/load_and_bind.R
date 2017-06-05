load_and_bind <- function(x, out_path, out_name){
  
  #browser()
  
  all_tiles <- loop(
    x, 
    fread,
    header = TRUE,
    sep = ",",
    na.strings = c("NA", "-1.#IND", "Peipsi", "Moskva", "IJsselmeer", "Zeeuwse meren"),
    fill = TRUE, 
    data.table = FALSE,
    parallel = TRUE)
  
  bb <- do.call("rbind", all_tiles)
  
  write_out_rds(bb, out_path, out_name)
  
}
