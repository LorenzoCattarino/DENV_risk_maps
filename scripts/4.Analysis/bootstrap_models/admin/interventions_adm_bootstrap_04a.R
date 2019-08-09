# For a general type of intervention, calculate

# how many countries are dengue free (R0 < 1), for each level of R0 reduction

library(dplyr)
library(countrycode)

source(file.path("R", "utility_functions.R"))
source(file.path("R", "create_parameter_list.R"))
source(file.path("R", "prepare_datasets", "average_up.R"))
source(file.path("R", "prepare_datasets", "calculate_mean_across_fits.R"))


# define parameters ----------------------------------------------------------- 


extra_prms <- list(id = 4,
                   R0_scenario = c(1, 2),
                   intervention_name = "wolbachia",
                   treatment_name = "scaling_factor",
                   phi_factor_levels = c("2S", "4S"))


# define variables ------------------------------------------------------------


parameters <- create_parameter_list(extra_params = extra_prms)

R0_scenario <- parameters$R0_scenario

intervention_name <- parameters$intervention_name

treatment_name <- parameters$treatment_name

phi_factor_levels <- parameters$phi_factor_levels

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
                            "country_level_R0")

fct_comb_fl_nm <- paste0("scenario_table_", intervention_name, ".csv")


# load data ------------------------------------------------------------------- 


fct_comb_ls <- read.csv(file.path(in_path, fct_comb_fl_nm),
                        stringsAsFactors = FALSE)

age_struct_orig <- read.csv(file.path("output", 
                                      "datasets",
                                      "country_age_structure.csv"), 
                            stringsAsFactors = FALSE) 


# pre processing -------------------------------------------------------------- 


age_struct_orig$continent <- as.factor(countrycode(sourcevar = age_struct_orig[, "country"], 
                                                   origin = "country.name", 
                                                   destination = "continent"))

age_struct_orig$region <- as.factor(countrycode(sourcevar = age_struct_orig[, "country"], 
                                                origin = "country.name", 
                                                destination = "region"))

# remove text in brackets 
nice_strings <- gsub("\\s*\\([^\\)]+\\)", "", age_struct_orig$country)

# remove text after comma
nice_strings_2 <- gsub("(.*),.*", "\\1", nice_strings)

# remove "*"
nice_strings_3 <- gsub("\\*", "", nice_strings_2)

age_struct_orig$country <- nice_strings_3

# keep only dengue endemic countries 
# age_struct <- inner_join(age_struct_orig, endemic_c[, "ID_0", drop = FALSE], by = "ID_0")  
age_struct <- age_struct_orig

col_names <- as.character(seq_len(parameters$no_samples))


# aggreggating ---------------------------------------------------------------- 


out_ls <- vector("list", length(R0_scenario))

for (k in seq_along(R0_scenario)){                         # loop over R0 assumptions
  
  cat("R0 assumption =", k, "\n")
  
  root_name <- sprintf("transformed_r_%s_%s_", k, intervention_name)
  
  my_in_path <- in_path
  my_out_path <- out_table_path
  
  my_fct_comb <- fct_comb_ls[-1,]
  
  my_fct_comb$phi_set_id <- k
  
  fi <- list.files(my_in_path, 
                   pattern = paste0("^", root_name),
                   full.names = TRUE)
  
  num.sort <- as.numeric(gsub(".*_|\\.rds$", "\\1", fi, perl = TRUE))
  
  fi_sort <- fi[order(num.sort)]
  
  # message(fi_sort)
  
  dat <- lapply(fi_sort, readRDS)
  
  small_out_ls <- vector("list", length(dat))
  small_out_ls_2 <- vector("list", length(dat))
  
  for (i in seq_along(dat)){                              # loop over treatments
    
    scenario_id <- my_fct_comb[i, "id"]
    cat("scenario table id =", scenario_id, "\n")
    
    one_dat <- as.data.frame(dat[[i]])
    
    average_sqr <- lapply(as.list(col_names), multi_col_average_up, one_dat, "ID_0")
    
    average_sqr_clean <- lapply(average_sqr, remove_pop_col)
    
    to_print_1 <- do.call("cbind", average_sqr_clean)
    to_print_2 <- average_boot_samples_dim2(to_print_1)
    to_print_3 <- cbind(ID_0 = average_sqr[[1]]$ID_0, to_print_2)
    to_print_4 <- inner_join(to_print_3, age_struct[, c("country", "ID_0")], by = "ID_0")
    write_out_csv(to_print_4,
                  my_out_path,
                  paste0("scenario_", scenario_id, ".csv"))
    
    dengue_free_c <- lapply(average_sqr_clean, how_many_below_1)
    
    dengue_free_c_mat <- unlist(dengue_free_c)
    
    ret <- average_boot_samples_dim1(dengue_free_c_mat)
    
    small_out_ls[[i]] <- ret
    
  }
  
  out_ls[[k]] <- cbind(my_fct_comb, do.call("rbind", small_out_ls))
  
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

summary_tab_fl_nm <- paste0("dengue_free_countries_",
                            intervention_name, 
                            ".csv")


# save table of baseline burden ---------------------------------------------


write_out_csv(summary_table, file.path("output", 
                                       "predictions_world", 
                                       "bootstrap_models",
                                       "adm_1"), 
              summary_tab_fl_nm)
