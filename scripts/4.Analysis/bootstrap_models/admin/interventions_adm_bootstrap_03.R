# For vaccine impact

# take the mean, sd and 95%CI across bootstrap samples, of
# total number of infections and cases, summed over all squares, AND:
# total number of infections and cases, summed by country 


library(dplyr)
library(countrycode)

source(file.path("R", "utility_functions.R"))
source(file.path("R", "create_parameter_list.R"))
source(file.path("R", "prepare_datasets", "calculate_mean_across_fits.R"))


# define parameters ----------------------------------------------------------- 


extra_prms <- list(id = 4,
                   R0_scenario = c(1, 2),
                   baseline_scenario_ids = 4,
                   intervention_name = "vaccine",
                   treatment_name = "screening_age",
                   phi_factor_levels = c("2S", "4S"),
                   base_info = c("population", 
                                 "ID_0", 
                                 "ID_1"))


# define variables ------------------------------------------------------------


parameters <- create_parameter_list(extra_params = extra_prms)

R0_scenario <- parameters$R0_scenario

baseline_scenario_ids <- parameters$baseline_scenario_ids 

treatment_name <- parameters$treatment_name

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

base_info <- parameters$base_info

out_ls <- vector("list", length(model_type))
out_ls_2 <- vector("list", length(model_type))


# load data ------------------------------------------------------------------- 


fct_comb <- read.csv(file.path(in_path, fct_comb_fl_nm), 
                     stringsAsFactors = FALSE)

age_struct <- read.csv(file.path("output", 
                                 "datasets",
                                 "country_age_structure.csv"), 
                       header = TRUE) 


# pre processing -------------------------------------------------------------- 


age_struct$continent <- as.factor(countrycode(sourcevar = age_struct[, "country"], 
                                              origin = "country.name", 
                                              destination = "continent"))

age_struct$region <- as.factor(countrycode(sourcevar = age_struct[, "country"], 
                                           origin = "country.name", 
                                           destination = "region"))

# remove text in brackets 
nice_strings <- gsub("\\s*\\([^\\)]+\\)", "", age_struct$country)

# remove text after comma
nice_strings_2 <- gsub("(.*),.*", "\\1", nice_strings)

# remove "*"
nice_strings_3 <- gsub("\\*", "", nice_strings_2)

age_struct$country <- nice_strings_3


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

    one_dat <- inner_join(one_dat, age_struct[, c("continent", "region", "country", "ID_0")])
    
    by_country <- one_dat %>% group_by(ID_0)
    country_sums <- by_country %>% summarise_at(var_to_sum, "sum")
    ret <- average_boot_samples_dim2(country_sums[, var_to_sum])
    ret <- round(ret, -2)
    ret2 <- cbind(ID_0 = country_sums$ID_0, ret)
    ret3 <- merge(age_struct[, c("country", "ID_0")], ret2, by = "ID_0", all.x = FALSE)
    write_out_csv(ret3, 
                  my_out_path, 
                  paste0(burden_measure, "_by_country_", scenario_id, ".csv"))
    
    by_continent <- one_dat %>% group_by(continent)
    continent_sums <- by_continent %>% summarise_at(var_to_sum, "sum")       
    ret <- average_boot_samples_dim2(continent_sums[, var_to_sum])
    ret <- round(ret, -2)
    ret2 <- cbind(continent = continent_sums$continent, ret)  
    write_out_csv(ret2, 
                  my_out_path, 
                  paste0(burden_measure, "_by_continent_", scenario_id, ".csv"))
    
    ret4 <- colSums(one_dat[, var_to_sum])
    ret5 <- average_boot_samples_dim1(ret4)
    ret5 <- round(ret5, -2)
    
    small_out_ls[[i]] <- ret5
    
    bl <- baseline[, var_to_sum]
    od <- one_dat[, var_to_sum]
    
    prop_red_pxl <- (bl - od) / bl
    prop_red_pxl[is.na(prop_red_pxl)] <- NA
    prop_red_pxl_2 <- cbind(one_dat[, base_info], prop_red_pxl)
    write_out_rds(prop_red_pxl_2, 
                  in_path,
                  sprintf("%s_pr_%s_%s_%s%s", out_file_tag, k, intervention_name, scenario_id, ".rds"))
    
    bl_colsum <- colSums(bl)
    od_colsum <- colSums(od)
    ret6 <- (bl_colsum - od_colsum) / bl_colsum
    ret7 <- average_boot_samples_dim1(ret6)
    
    small_out_ls_2[[i]] <- ret7
    
  }
  
  out_ls[[k]] <- cbind(my_fct_comb, do.call("rbind", small_out_ls))
  out_ls_2[[k]] <- cbind(my_fct_comb, do.call("rbind", small_out_ls_2))
  
}

summary_table <- do.call("rbind", out_ls)  

names(summary_table)[names(summary_table) == treatment_name] <- "treatment"

treatment_levels <- unique(summary_table$treatment)

summary_table[, "treatment"] <- factor(summary_table[, "treatment"],
                                       levels = treatment_levels,
                                       labels = treatment_levels)

summary_table$phi_set_id <- factor(summary_table$phi_set_id, 
                                   levels = seq_len(length(phi_factor_levels)), 
                                   labels = phi_factor_levels)

summary_tab_fl_nm <- paste0("total_", intervention_name, ".csv")

write_out_csv(summary_table, file.path("output", 
                                       "predictions_world", 
                                       "bootstrap_models",
                                       "adm_1"), 
              summary_tab_fl_nm,
              row.names = FALSE)

summary_table_2 <- do.call("rbind", out_ls_2)  

names(summary_table_2)[names(summary_table_2) == treatment_name] <- "treatment"

treatment_levels <- unique(summary_table_2$treatment)

summary_table_2[, "treatment"] <- factor(summary_table_2[, "treatment"],
                                         levels = treatment_levels,
                                         labels = treatment_levels)

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
