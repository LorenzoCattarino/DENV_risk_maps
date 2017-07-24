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
  
  out_type <- sub(".*(?=.{3}$)", "", out_name, perl = T)
  
  if (out_type == "rds"){
    
    write_out_rds(bb, out_path, out_name)
  
  }
  
  if (out_type == "txt"){
    
    dir.create(out_path, FALSE, TRUE)
  
    write.table(all_preds, 
                file.path(out_path, out_name),
                row.names = FALSE,
                sep = ",")
  }
  
}
