mean_grouped_data <- function(i, yy) sum(i * yy)

sd_grouped_data <- function(i, age_dat, mean_vals) {
  
  (sqrt(sum((yy^2) * age_dat[i,]) - mean_vals[i]^2))

}
