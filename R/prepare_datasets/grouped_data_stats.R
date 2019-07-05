mean_grouped_data <- function(i, yy) sum(i * yy)

sd_grouped_data <- function(i, age_dat, mean_vals) {
  
  (sqrt(sum((yy^2) * age_dat[i,]) - mean_vals[i]^2))

}

edit_age_band_tags <- function(age_distr_df) {
  
  age_band_tgs <- grep("band", names(age_distr_df), value = TRUE)
  
  age_bounds_num <- sub("^[^_]+_", "", age_band_tgs)
  
  sub("_", "-", age_bounds_num)
  
}