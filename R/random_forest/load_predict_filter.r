load_predict_filter <- function(
  i, ids_vec, in_path,
  predictors, RF_obj, foi_dts){
  
  #browser()
  
  one_id <- ids_vec[i]
  cat("tile id =", one_id, "\n")
  
  file_name <- paste0("tile_", one_id, ".txt")
  
  tile <- fread(file.path(in_path, file_name),
                header = TRUE,
                sep = ",",
                na.strings = c("NA", "-1.#IND", "Peipsi", "Moskva", "IJsselmeer", "Zeeuwse meren"),
                fill = TRUE,
                data.table = FALSE)
  
  xx <- remove_NA_rows(tile, predictors)
  
  xx$pred <- make_h2o_predictions(RF_obj, xx, predictors)
  
  if(is.character(xx$ADM_0)) stop("ADM_0 is a character")
  
  a <- "ADM_1"
  
  zz <- subset(xx, xx[[a]] != -1)
  
  aa <- inner_join(
    zz, 
    foi_dts[, c("data_id", "ADM_0", "ADM_1")])
  
  subset(aa, population != 0)
  
}  
