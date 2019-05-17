average_up <- function(pxl_df, grp_flds, var_names){
  
  # browser()
  
  by_grp <- pxl_df %>% group_by(.dots = grp_flds)
  
  wtd_mean_pixel_data <- by_grp %>% summarise_at(var_names,
    funs(weighted.mean(., population, na.rm = TRUE)))
  
  mean_pixel_data <- by_grp %>% summarise(
    population = sum(population))
  
  aggreg_pixel_data <- left_join(wtd_mean_pixel_data, mean_pixel_data)
  
  as.data.frame(aggreg_pixel_data)

}

average_data_points <- function(i){
  
  i %>%
    group_by(ADM_0, ADM_1) %>%
    summarise(o_j = mean(o_j), admin = mean(admin), square = mean(square))
}

multi_col_average_up <- function(i, x, grp_flds){
  
  dat <- x[, c("population", grp_flds, i)]
  average_up(dat, grp_flds, i)

}

remove_pop_col <- function(i){
  i[, setdiff(names(i), c("ID_0", "population"))]
}

how_many_below_1 <- function(x){
  
  sum(x < 1)  
  
}
