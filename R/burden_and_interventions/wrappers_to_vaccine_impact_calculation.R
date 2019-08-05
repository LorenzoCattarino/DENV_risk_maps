wrapper_to_multi_factor_vaccine_impact <- function(x, 
                                                   preds, 
                                                   parms, 
                                                   out_path){
  
  run_ID <- x$id
  screen_age <- x$screening_age
  phi_set_id <- x$phi_set_id
  burden_measure <- x$burden_measure
  estimate <- x$estimate
  
  cat("ID run =", run_ID, "\n")
  cat("screening age =", screen_age, "\n")
  cat("R0 assumption =", phi_set_id, "\n")
  cat("burden measure =", burden_measure, "\n")
  cat("vaccine estimate =", estimate, "\n")
  
  base_info <- parms$base_info
  no_fits <- parms$no_samples
  w_scenario_id <- parms$wolbachia_scenario_id
  R0_scenario <- parms$R0_scenario
  parallel_2 <- parms$parallel_2
  
  out_file_tag <- toupper(substr(burden_measure, 1, 1))
  
  burden_file_name <- paste0(out_file_tag, "_num_wolbachia_", w_scenario_id, "_fixed.rds")
  
  lookup_table_nm <- sprintf("R0_to_prop_%s_averted_lookup_%s_%s%s", 
                             burden_measure, 
                             R0_scenario, 
                             estimate,
                             ".csv")
  
  model_type <- paste0("model_", parms$id)
  
  if(base_info[1] == "cell"){
    
    fl_pt <- file.path("output", 
                       "predictions_world", 
                       "bootstrap_models",
                       model_type)
    
  } else {
    
    fl_pt <- file.path("output", 
                       "predictions_world", 
                       "bootstrap_models",
                       model_type,
                       "adm_1")    
    
  }
  
  
  # load data -----------------------------------------------------------------  
  

  look_up_table <- read.csv(file.path("data",
                                      "vaccine",
                                      "vaccine_lookup_table",
                                      lookup_table_nm),
                            header = TRUE)
  
  burden <- readRDS(file.path(fl_pt, burden_file_name))
  
  
  # ---------------------------------------------------------------------------
  
  
  col_ids <- as.character(seq_len(no_fits))
  
  look_up_table <- look_up_table[,-1]
  
  max_R0_to_lookup <- ceiling(max(preds[, col_ids]))
  
  new_first_row <- cbind(R0 = 0, look_up_table[1, 2:18])
  new_last_row <- cbind(R0 = max_R0_to_lookup, look_up_table[nrow(look_up_table), 2:18])
  
  look_up_table_2 <- rbind(new_first_row, look_up_table, new_last_row)
  
  look_up_table_2 <- as.matrix(look_up_table_2)
  
  out_fl_nm <- paste0(out_file_tag, "_num_", R0_scenario, "_vaccine_", run_ID, ".rds")

  prop_averted <- loop(
    seq_len(nrow(preds)),
    wrapper_to_replicate_vaccine_impact, 
    parms = parms,
    preds = preds, 
    vaccine_lookup = look_up_table_2,
    screen_age = screen_age,
    parallel = parallel_2)
  
  output1 <- lapply(prop_averted, "[[", 1)
  
  output2 <- lapply(prop_averted, "[[", 2)
  
  prop_averted <- do.call("rbind", output1)
  
  burden_net_vaccine <- (1 - prop_averted) * burden[, col_ids]

  out <- cbind(burden[, base_info], burden_net_vaccine)
  
  write_out_rds(as.data.frame(out), out_path, out_fl_nm)
  
  if(is.null(screen_age)){
    
    ages_max_impact <- do.call("rbind", output2)    
    
    ages_max_impact <- ages_max_impact + 1
    
    colnames(ages_max_impact) <- col_ids
    
    out_fl_nm <- paste0(out_file_tag, "_num_", R0_scenario, "_max_age_vaccine_", run_ID, ".rds")
    
    out <- cbind(burden[, base_info], ages_max_impact)
    
    write_out_rds(as.data.frame(out), out_path, out_fl_nm)
    
  }
}

wrapper_to_replicate_vaccine_impact <- function(i,
                                                parms,
                                                preds, 
                                                vaccine_lookup,
                                                screen_age = NULL){
  
  approx_all_ages <- function(j, vaccine_lookup, preds){
    approx(vaccine_lookup[, "R0"], vaccine_lookup[, j], xout = preds)$y
  }
  
  no_fits <- parms$no_samples
  
  col_ids <- as.character(seq_len(no_fits))
  
  preds_i <- preds[i, col_ids]
  
  if(!is.null(screen_age)){
    
    out <- approx(vaccine_lookup[, "R0"], vaccine_lookup[, screen_age], xout = preds_i)$y
    
    out_2 <- NULL
    
  } else {
    
    all_ages <- seq_len(18)
    
    # look up reduction for each age
    ret_all_ages <- vapply(all_ages[2:length(all_ages)], approx_all_ages, numeric(no_fits), vaccine_lookup, preds_i)
    
    # find max reduction across ages (columns)
    out <- rowMaxs(ret_all_ages) 
    
    # find age of max vaccine impact
    out_2 <- max.col(ret_all_ages)
    
  }
  
  list(out, out_2)

}
