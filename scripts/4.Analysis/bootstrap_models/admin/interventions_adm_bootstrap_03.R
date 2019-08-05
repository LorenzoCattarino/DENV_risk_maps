# When optimally picking screening age:

# take the mean, sd and 95%CI across bootstrap samples, of
# total number of infections and cases, summed over all squares, AND:
# total number of infections and cases, summed by country 

library(dplyr)

source(file.path("R", "utility_functions.R"))
source(file.path("R", "create_parameter_list.R"))
source(file.path("R", "prepare_datasets", "calculate_mean_across_fits.R"))


# define parameters ----------------------------------------------------------- 


extra_prms <- list(id = 4,
                   R0_scenario = c(1, 2),
                   baseline_scenario_ids = 4,
                   intervention_name = "vaccine",
                   phi_factor_levels = c("2S", "4S"))


# define variables ------------------------------------------------------------


parameters <- create_parameter_list(extra_params = extra_prms)

R0_scenario <- parameters$R0_scenario

baseline_scenario_ids <- parameters$baseline_scenario_ids 

model_type <- paste0("model_", parameters$id)

intervention_name <- parameters$intervention_name

phi_factor_levels <- parameters$phi_factor_levels

in_path <- file.path("output", 
                     "predictions_world", 
                     "bootstrap_models", 
                     model_type,
                     "adm_1")

var_to_sum <- as.character(seq_len(parameters$no_samples))

out_table_path <- file.path("output", 
                            "predictions_world", 
                            "bootstrap_models", 
                            model_type,
                            "adm_1",
                            intervention_name)

out_fig_path <- file.path("figures", 
                          "predictions_world", 
                          "bootstrap_models",
                          "adm_1")

fct_comb_fl_nm <- paste0("scenario_table_", intervention_name, ".csv")

out_ls <- vector("list", length(model_type))
out_ls_2 <- vector("list", length(model_type))


# load data ------------------------------------------------------------------- 


fct_comb <- read.csv(file.path(in_path, fct_comb_fl_nm), 
                     stringsAsFactors = FALSE)

age_struct <- read.csv(file.path("output", 
                                 "datasets",
                                 "country_age_structure.csv"), 
                       header = TRUE) 


# aggreaggating --------------------------------------------------------------- 


for (k in seq_along(R0_scenario)){                                  # loop over R0 assumptions
  
  cat("R0 assumption =", k, "\n")
  
  my_in_path <- in_path
  my_out_path <- out_table_path
  
  my_fct_comb <- fct_comb
  
  my_fct_comb$phi_set_id <- k
  
  small_out_ls <- vector("list", length(nrow(my_fct_comb)))
  small_out_ls_2 <- vector("list", length(nrow(my_fct_comb)))
  
  for (i in seq_len(nrow(my_fct_comb))){                           # loop over scenario ids
    
    scenario_id <- my_fct_comb[i, "id"]
    cat("scenario table id =", scenario_id, "\n")
    
    burden_measure <- my_fct_comb[i, "burden_measure"]
    
    out_file_tag <- toupper(substr(burden_measure, 1, 1))
    
    root_name <- sprintf("%s_num_%s_%s_%s%s", out_file_tag, k, intervention_name, scenario_id, ".rds")
    
    baseline_id <- baseline_scenario_ids
    baseline_fl_nm <- paste0(out_file_tag, "_num_wolbachia_", baseline_id, "_fixed.rds")
    baseline <- readRDS(file.path(my_in_path, baseline_fl_nm)) 
    
    dat <- readRDS(file.path(my_in_path, root_name))
    
    one_dat <- as.data.frame(dat)

    bl <- baseline[, var_to_sum]
    od <- one_dat[, var_to_sum]
    
    bl_colsum <- colSums(bl)
    od_colsum <- colSums(od)
    ret6 <- (bl_colsum - od_colsum) / bl_colsum
    ret7 <- average_boot_samples_dim1(ret6)
    
    small_out_ls_2[[i]] <- ret7
    
  }
  
  out_ls_2[[k]] <- cbind(my_fct_comb, do.call("rbind", small_out_ls_2))
  
}

summary_table_2 <- do.call("rbind", out_ls_2)  

summary_table_2$phi_set_id <- factor(summary_table_2$phi_set_id, 
                                     levels = seq_len(length(phi_factor_levels)), 
                                     labels = phi_factor_levels)

summary_tab_fl_nm_2 <- paste0("prop_change_", intervention_name, ".csv")

write_out_csv(summary_table_2, file.path("output", 
                                         "predictions_world", 
                                         "bootstrap_models",
                                         "adm_1"), 
              summary_tab_fl_nm_2,
              row.names = FALSE)


# subdivide by vaccine estimate -----------------------------------------------


vacc_estimates <- unique(summary_table_2$estimate)

burden_measures <- unique(summary_table_2$burden_measure)

for (j in seq_along(burden_measures)) {
  
  for (i in seq_along(vacc_estimates)) {
    
    bur_meas <- burden_measures[j]
    
    vacc_est <- vacc_estimates[i]
    
    summary_table_2_sub <- subset(summary_table_2, 
                                  burden_measure == bur_meas & estimate == vacc_est)
    
    summary_tab_fl_nm <- sprintf("prop_change_%s_%s_%s%s", 
                                 bur_meas, vacc_est, intervention_name, ".csv")
    
    write_out_csv(summary_table_2_sub, file.path("output", 
                                                 "predictions_world", 
                                                 "bootstrap_models",
                                                 "adm_1"), 
                  summary_tab_fl_nm,
                  row.names = FALSE)
  }
  
}
