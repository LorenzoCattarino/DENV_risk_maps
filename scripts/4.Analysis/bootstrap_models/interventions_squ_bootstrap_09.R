# For a general type of intervention, calculate

# how many countries are dengue free (R0 < 1), for each level of R0 reduction

library(dplyr)
library(data.table)
library(countrycode)

source(file.path("R", "prepare_datasets", "calculate_mean_across_fits.R"))
source(file.path("R", "utility_functions.R"))
source(file.path("R", "prepare_datasets", "average_up.R"))


# define parameters ----------------------------------------------------------- 


parameters <- list(
  id = c(22, 23, 24),
  no_samples = 200,
  baseline_scenario_ids = c(1, 2, 3))   

intervention_name <- "wolbachia"

treatment_name <- "scaling_factor"

phi_factor_levels <- c("2S", "4S", "4S(sym = 2x asym)")


# define variables ------------------------------------------------------------


baseline_scenario_ids <- parameters$baseline_scenario_ids 

model_type <- paste0("model_", parameters$id)

in_path <- file.path("output", 
                     "predictions_world", 
                     "bootstrap_models", 
                     model_type)

var_to_sum <- as.character(seq_len(parameters$no_samples))

out_table_path <- file.path("output",
                            "predictions_world",
                            "bootstrap_models",
                            model_type,
                            "country_level_R0")

fct_comb_fl_nm <- paste0("scenario_table_", intervention_name, ".csv")


# load data ------------------------------------------------------------------- 


fct_comb_ls <- lapply(file.path(in_path, fct_comb_fl_nm), read.csv, header = TRUE)

age_struct_orig <- read.csv(file.path("output", 
                                      "datasets",
                                      "country_age_structure.csv"), 
                            stringsAsFactors = FALSE) 

endemic_c <- read.csv(file.path("output", 
                                "datasets", 
                                "dengue_endemic_countries.csv"),
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
age_struct <- inner_join(age_struct_orig, endemic_c[, "ID_0", drop = FALSE], by = "ID_0")  


# aggreggating ---------------------------------------------------------------- 


out_ls <- vector("list", length(model_type))

root_name <- paste0("response_r_", intervention_name, "_")

for (k in seq_along(model_type)){                         # loop over R0 assumptions
  
  cat("R0 assumption =", k, "\n")
  
  # index <- k + (length(in_path)*(j-1))
  
  my_in_path <- in_path[k]
  my_out_path <- out_table_path[k]
  
  my_fct_comb <- fct_comb_ls[[k]]
  
  fi <- list.files(my_in_path, 
                   pattern = paste0("^", root_name),
                   full.names = TRUE)
  
  num.sort <- as.numeric(gsub(".*_|\\.rds$", "\\1", fi, perl = TRUE))
  
  fi_sort <- fi[order(num.sort)]
  
  message(fi_sort)
  
  dat <- lapply(fi_sort, readRDS)
  
  small_out_ls <- vector("list", length(dat))
  small_out_ls_2 <- vector("list", length(dat))
  
  # baseline_id <- baseline_scenario_ids[k]
  # baseline <- readRDS(file.path(my_in_path, paste0(root_name, "_", baseline_id, ".rds"))) 
  
  for (i in seq_along(dat)){                              # loop over treatments
    
    scenario_id <- my_fct_comb[i, "id"]
    cat("scenario table id =", scenario_id, "\n")
    
    one_dat <- as.data.frame(dat[[i]])
    # one_dat <- inner_join(one_dat, age_struct[, c("continent", "region", "country", "ID_0")])
    
    col_names <- as.character(seq_len(parameters$no_samples))
      
    average_sqr <- lapply(as.list(col_names), multi_col_average_up, one_dat, "ID_0")
    
    average_sqr_clean <- lapply(average_sqr, remove_pop_col)
    
    to_print_1 <- do.call("cbind", average_sqr_clean)
    to_print_2 <- average_boot_samples_dim2(to_print_1)
    to_print_3 <- cbind(ID_0 = average_sqr[[1]]$ID_0, to_print_2)
    to_print_4 <- inner_join(to_print_3, age_struct[, c("country", "ID_0")])
    write_out_csv(to_print_4,
                  my_out_path,
                  paste0("scenario_", scenario_id, ".csv"))
    
    dengue_free_c <- lapply(average_sqr_clean, how_many_below_1)
    
    dengue_free_c_mat <- unlist(dengue_free_c)
    
    ret <- average_boot_samples_dim1(dengue_free_c_mat)
    # ret <- round(ret, -2)
    # ret2 <- cbind(ID_0 = country_sums$ID_0, ret)
    # ret3 <- merge(age_struct[, c("country", "ID_0")], ret2, by = "ID_0", all.x = FALSE)
    # write_out_csv(ret, 
    #               my_out_path, 
    #               paste0(my_var_name, "_by_country_", scenario_id, ".csv"))
    
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
                                       "bootstrap_models"), 
              summary_tab_fl_nm)

