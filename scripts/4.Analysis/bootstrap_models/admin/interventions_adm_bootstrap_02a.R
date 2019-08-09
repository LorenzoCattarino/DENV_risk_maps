# For wolbachia impact

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
                   burden_measures = c("infections", "cases", "hosp"),
                   baseline_scenario_ids = 4,
                   intervention_name = "wolbachia",
                   treatment_name = "scaling_factor",
                   phi_factor_levels = c("2S", "4S"),
                   base_info = c("population", 
                                 "ID_0", 
                                 "ID_1"))


# define variables ------------------------------------------------------------


parameters <- create_parameter_list(extra_params = extra_prms)

R0_scenario <- parameters$R0_scenario

baseline_scenario_ids <- parameters$baseline_scenario_ids 

burden_measures <- parameters$burden_measures

intervention_name <- parameters$intervention_name

treatment_name <- parameters$treatment_name

phi_factor_levels <- parameters$phi_factor_levels

vars <- toupper(substr(burden_measures, 1, 1))

model_type <- paste0("model_", parameters$id)

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

fct_comb_fl_nm <- paste0("scenario_table_", intervention_name, ".csv")

base_info <- parameters$base_info


# load data ------------------------------------------------------------------- 


fct_combs <- read.csv(file.path(in_path, fct_comb_fl_nm), 
                      stringsAsFactors = FALSE)

age_struct <- read.csv(file.path("output", 
                                 "datasets",
                                 "country_age_structure.csv"), 
                       stringsAsFactors = FALSE) 


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

# bs_line_out <- vector("list", length(vars))


# aggreggating ---------------------------------------------------------------- 


for (j in seq_along(vars)){                                 # loop over burden measures 
  
  out_ls <- vector("list", length(R0_scenario))
  out_ls_2 <- vector("list", length(R0_scenario))
  
  my_var_name <- burden_measures[j]
  cat("burden measure =", my_var_name, "\n")
  
  my_var <- vars[j]
  
  my_in_path <- in_path
  
  baseline_id <- baseline_scenario_ids
  
  baseline_fl_nm <- sprintf("%s_num_%s_%s%s", my_var, intervention_name, baseline_id, ".rds")
  
  baseline <- as.data.frame(readRDS(file.path(my_in_path, baseline_fl_nm))) 
  
  for (k in seq_along(R0_scenario)){                         # loop over R0 assumptions
    
    cat("R0 assumption =", k, "\n")
    
    # index <- k + (length(in_path)*(j-1))
    
    root_name <- sprintf("%s_num_%s_%s", my_var, k, intervention_name)
    
    my_out_path <- file.path(out_table_path, paste0("R0_assumption_", k))
    
    my_fct_comb <- fct_combs[-1,]
    
    my_fct_comb$phi_set_id <- k
    
    fi <- file.path(my_in_path, paste0(root_name, "_", my_fct_comb$id, ".rds"))
    
    # message(fi)
    
    dat <- lapply(fi, readRDS)
    
    small_out_ls <- vector("list", length(dat))
    small_out_ls_2 <- vector("list", length(dat))
    
    for (i in seq_along(dat)){                              # loop over treatments
      
      scenario_id <- my_fct_comb[i, "id"]
      cat("scenario table id =", scenario_id, "\n")
      
      one_dat <- as.data.frame(dat[[i]])
      
      one_dat <- inner_join(one_dat, age_struct[, c("continent", "region", "country", "ID_0")])
      
      by_country <- one_dat %>% group_by(ID_0)
      country_sums <- by_country %>% summarise_at(var_to_sum, "sum")
      ret <- average_boot_samples_dim2(country_sums[, var_to_sum])
      ret <- round(ret, -2)
      ret2 <- cbind(ID_0 = country_sums$ID_0, ret)
      ret3 <- merge(age_struct[, c("country", "ID_0")], ret2, by = "ID_0", all.x = FALSE)
      write_out_csv(ret3, 
                    my_out_path, 
                    paste0(my_var_name, "_by_country_", scenario_id, ".csv"),
                    row.names = FALSE)
      
      by_continent <- one_dat %>% group_by(continent)
      continent_sums <- by_continent %>% summarise_at(var_to_sum, "sum")       
      ret <- average_boot_samples_dim2(continent_sums[, var_to_sum])
      ret <- round(ret, -2)
      ret2 <- cbind(continent = continent_sums$continent, ret)  
      write_out_csv(ret2, 
                    my_out_path, 
                    paste0(my_var_name, "_by_continent_", scenario_id, ".csv"),
                    row.names = FALSE)
      
      ret4 <- colSums(one_dat[, var_to_sum])
      ret5 <- average_boot_samples_dim1(ret4)
      ret5 <- round(ret5, -2)
      
      small_out_ls[[i]] <- ret5
      
      bl <- baseline[, var_to_sum]
      od <- one_dat[, var_to_sum]
      
      prop_red_pxl <- (bl - od) / bl
      prop_red_pxl[is.na(prop_red_pxl)] <- 0
      prop_red_pxl_2 <- cbind(one_dat[, base_info], prop_red_pxl)
      write_out_rds(prop_red_pxl_2, 
                    in_path,
                    sprintf("%s_pr_%s_%s_%s%s", my_var, k, intervention_name, scenario_id, ".rds"))
      
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
  
  summary_tab_fl_nm <- paste0("total_", 
                              my_var_name, 
                              "_", 
                              intervention_name, 
                              ".csv")
  
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
  
  summary_tab_fl_nm_2 <- paste0("prop_change_", 
                                my_var_name, 
                                "_", 
                                intervention_name, 
                                ".csv")
  
  write_out_csv(summary_table_2, file.path("output", 
                                           "predictions_world", 
                                           "bootstrap_models",
                                           "adm_1"), 
                summary_tab_fl_nm_2,
                row.names = FALSE)
  
  
  # summarise the baseline dataset --------------------------------------------
  
  
  # # by country
  # one_dat <- inner_join(baseline, age_struct[, c("continent", "region", "country", "ID_0")])
  # by_country <- one_dat %>% group_by(ID_0)
  # country_sums <- by_country %>% summarise_at(var_to_sum, "sum")
  # ret <- average_boot_samples_dim2(country_sums[, var_to_sum])
  # ret <- round(ret, -2)
  # ret2 <- cbind(ID_0 = country_sums$ID_0, ret)
  # ret3 <- merge(age_struct[, c("country", "ID_0")], ret2, by = "ID_0", all.x = FALSE)
  # write_out_csv(ret3, 
  #               out_table_path, 
  #               paste0(my_var_name, "_by_country_", baseline_id, ".csv"),
  #               row.names = FALSE)
  # # overall
  # total_burden_baseline <- average_boot_samples_dim1(bl_colsum)
  # total_burden_baseline <- round(total_burden_baseline, -2)
  # total_burden_baseline <- t(data.frame(total_burden_baseline))
  # rownames(total_burden_baseline) <- burden_measures[j]
  # 
  # bs_line_out[[j]] <- total_burden_baseline
  
}  


# # save table of baseline burden -----------------------------------------------
# 
# 
# baseline_summary_table <- do.call("rbind", bs_line_out) 
# 
# basel_burden_fl_nm <- paste0("baseline_total_", intervention_name, ".csv")
# 
# write_out_csv(baseline_summary_table, file.path("output", 
#                                                 "predictions_world", 
#                                                 "bootstrap_models"), 
#               basel_burden_fl_nm, 
#               row.names = TRUE)
