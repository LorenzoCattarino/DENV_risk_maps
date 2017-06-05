average_up <- function(pxl_df, grp_flds, var_names){
  
  #browser()
  
  by_grp <- pxl_df %>% group_by_(.dots = grp_flds)
  
  wtd_mean_pixel_data <- by_grp %>% summarise_each(
    funs(weighted.mean(., population, na.rm = TRUE)), 
    one_of(var_names))
  
  mean_pixel_data <- by_grp %>% summarise(
    population = sum(population))
  
  aggreg_pixel_data <- left_join(wtd_mean_pixel_data, mean_pixel_data)
  
  as.data.frame(aggreg_pixel_data)

}
