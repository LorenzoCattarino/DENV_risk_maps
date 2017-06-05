wrapper_to_weight_calc <- function(
  disaggr_dataset, grp_flds, FOI_s){
  
  pred_by_grp <- disaggr_dataset %>% group_by_(.dots = grp_flds)
  
  sum_p_st <- pred_by_grp %>% summarise(sum_p_st = sum(population))
    
  sum_p_st_times_y_st <- pred_by_grp %>% summarise(sum_p_st_times_y_st = sum(population * FOI))
  
  join_1 <- left_join(FOI_s, sum_p_st)
  
  join_2 <- left_join(join_1, sum_p_st_times_y_st)
  
  zero_denom_log <- join_2$sum_p_st_times_y_st == 0
  
  join_2$wgts <- 1
  
  tar <- as.matrix(join_2[!zero_denom_log, ])
  
  wgts <- apply(
    tar,
    1,
    function(x) calculate_weight(x["FOI_s"], x["sum_p_st"], x["sum_p_st_times_y_st"]))

  if(sum(is.na(wgts)) > 0){
    browser()
  }
  
  join_2[!zero_denom_log, "wgts"] <- wgts

  new_FOI_wgts <- left_join(
    disaggr_dataset[, c("ID_0", "ID_1", "FOI")],
    join_2[, c("ID_0", "ID_1", "wgts")])
  
  new_FOI_wgts$wgts
}
