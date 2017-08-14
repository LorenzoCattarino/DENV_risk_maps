append_ISO_codes <- function(i, ids_vec){
  
  one_id <- ids_vec[i]
  cat("tile id = ", one_id, "\n")
  
  vars <- c("ADM_0", "ADM_1", "ADM_2")
    
  join_field <- "pixel_id" 
    
  file_name <- paste0("tile_", one_id, ".txt")
  
  tile_a <- fread(file_name)  
  
  tile_b <- fread(file.path("accel2", file_name))
  
  tile_b <- subset(tile_b,, c(join_field, vars))
  
  tile_c <- merge(tile_a, tile_b, by = join_field, all.x = TRUE)
  
  # create output dir 
  dir.create("tiles_2", FALSE, TRUE)
  
  write.table(tile_c, 
              file.path("tiles_2", file_name), 
              row.names = FALSE, sep = ",")
  
}
