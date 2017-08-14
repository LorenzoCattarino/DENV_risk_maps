get_age_band_bounds <- function(tags) {
  
  age_band_tags_split <- strsplit(tags, "[^0-9]+")
  
  age_band_tags_split_num <- lapply(age_band_tags_split, as.numeric)
  
  age_band_tags_split_num_mat <- do.call("rbind", age_band_tags_split_num)
  
  output <- age_band_tags_split_num_mat[, 2:3]
  
  output
  
}
