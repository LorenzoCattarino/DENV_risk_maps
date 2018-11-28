# When optimally picking screening age:

# 1) take the mean, sd and 95%CI across bootstrap samples, of
# total number of infections and cases, summed over all squares, AND:
# total number of infections and cases, summed by country 

# 2) create barplots of total numbers

library(dplyr)
library(data.table)
library(ggplot2)
library(countrycode)

source(file.path("R", "prepare_datasets", "calculate_mean_across_fits.R"))
source(file.path("R", "utility_functions.R"))


# define parameters ----------------------------------------------------------- 


parameters <- list(
  id = c(22, 23, 24),
  no_samples = 200,
  desired_n_int = c(8, 6, 5),
  baseline_scenario_ids = c(1, 2, 3))   

intervention_name <- "vaccine"

phi_factor_levels <- c("2S", "4S", "4S(sym = 2x asym)")


# define variables ------------------------------------------------------------


baseline_scenario_ids <- parameters$baseline_scenario_ids 

desired_n_int <- parameters$desired_n_int

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

out_fig_path <- file.path("figures", 
                          "predictions_world", 
                          "bootstrap_models",
                          "adm_1")

fct_comb_fl_nm <- paste0("scenario_table_", intervention_name, ".csv")

out_ls <- vector("list", length(model_type))
out_ls_2 <- vector("list", length(model_type))


# load data ------------------------------------------------------------------- 


fct_comb_ls <- lapply(file.path(in_path, fct_comb_fl_nm), read.csv, header = TRUE)

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


# aggreaggating --------------------------------------------------------------- 


for (k in seq_along(model_type)){                                  # loop over R0 assumptions
  
  cat("R0 assumption =", k, "\n")
  
  my_in_path <- in_path[k]
  my_out_path <- out_table_path[k]
  
  my_fct_comb <- fct_comb_ls[[k]]
  
  small_out_ls <- vector("list", length(nrow(my_fct_comb)))
  small_out_ls_2 <- vector("list", length(nrow(my_fct_comb)))
  
  for (i in seq_len(nrow(my_fct_comb))){                           # loop over scenario ids
    
    scenario_id <- my_fct_comb[i, "id"]
    cat("scenario table id =", scenario_id, "\n")
    
    burden_measure <- my_fct_comb[i, "burden_measure"]
    
    out_file_tag <- toupper(substr(burden_measure, 1, 1))
    
    root_name <- paste0(out_file_tag, "_num_", intervention_name, "_", scenario_id, ".rds")
    
    baseline_id <- baseline_scenario_ids[k]
    baseline_fl_nm <- paste0(out_file_tag, "_num_wolbachia_", baseline_id, ".rds")
    baseline <- readRDS(file.path(my_in_path, baseline_fl_nm)) 
    
    dat <- readRDS(file.path(my_in_path, root_name))
    
    one_dat <- as.data.frame(dat)
    one_dat <- left_join(one_dat, age_struct[, c("continent", "region", "country", "ID_0")])
    
    by_country <- one_dat %>% group_by(ID_0)
    country_sums <- by_country %>% summarise_at(var_to_sum, "sum")
    ret <- average_boot_samples_dim2(country_sums[, var_to_sum])
    ret2 <- cbind(ID_0 = country_sums$ID_0, ret)
    ret3 <- merge(age_struct[, c("country", "ID_0")], ret2, by = "ID_0", all.x = FALSE)
    write_out_csv(ret3, 
                  my_out_path, 
                  paste0(burden_measure, "_by_country_", scenario_id, ".csv"))
    
    by_continent <- one_dat %>% group_by(continent)
    continent_sums <- by_continent %>% summarise_at(var_to_sum, "sum")       
    ret <- average_boot_samples_dim2(continent_sums[, var_to_sum])
    ret2 <- cbind(continent = continent_sums$continent, ret)  
    write_out_csv(ret2, 
                  my_out_path, 
                  paste0(burden_measure, "_by_continent_", scenario_id, ".csv"))
    
    by_region <- one_dat %>% group_by(region)
    region_sums <- by_region %>% summarise_at(var_to_sum, "sum")
    ret <- average_boot_samples_dim2(region_sums[, var_to_sum])
    ret2 <- cbind(region = region_sums$region, ret)  
    write_out_csv(ret2, 
                  my_out_path, 
                  paste0(burden_measure, "_by_region_", scenario_id, ".csv"))
    
    ret4 <- colSums(one_dat[, var_to_sum])
    ret5 <- average_boot_samples_dim1(ret4)
    
    small_out_ls[[i]] <- ret5
    
    bl <- baseline[, var_to_sum]
    od <- one_dat[, var_to_sum]
    
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

summary_table$phi_set_id <- factor(summary_table$phi_set_id, 
                                   levels = seq_len(length(phi_factor_levels)), 
                                   labels = phi_factor_levels)

summary_table_2 <- do.call("rbind", out_ls_2)  

summary_table_2$phi_set_id <- factor(summary_table_2$phi_set_id, 
                                     levels = seq_len(length(phi_factor_levels)), 
                                     labels = phi_factor_levels)


# plotting ------------------------------------------------------------------


burden_measures <- c("infections", "cases", "hosp") 

for (j in seq_along(burden_measures)) {
  
  bur_meas <- burden_measures[j]
  
  summary_table_sub <- subset(summary_table, burden_measure == bur_meas)
  
  summary_table_2_sub <- subset(summary_table_2, burden_measure == bur_meas)
  summary_tab_fl_nm <- paste0("prop_change_", bur_meas, "_", intervention_name, ".csv")
  write_out_csv(summary_table_2_sub, file.path("output", 
                                               "predictions_world", 
                                               "bootstrap_models",
                                               "adm_1"), 
                summary_tab_fl_nm)
  
  y_values <- pretty(0:max(summary_table_sub$mean), desired_n_int[j])
  max_y_value <- max(summary_table_sub$uCI)
  
  p <- ggplot(summary_table_sub, aes(bur_meas, mean, ymin = lCI, ymax = uCI)) + 
    geom_bar(stat = "identity", position = "dodge", width = 0.5, fill = "lightskyblue3") +
    geom_errorbar(width = .1, position = position_dodge(.9)) +
    facet_grid(. ~ phi_set_id) +
    xlab(NULL) +
    scale_y_continuous("Mean (95% CI)", 
                       breaks = y_values, 
                       labels = format(y_values/1000000), 
                       limits = c(min(y_values), max_y_value)) +
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.y = element_text(size = 12),
          plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
          strip.text.x = element_text(size = 8))
  
  dir.create(out_fig_path, FALSE, TRUE)
  
  barplot_fl_nm <- paste0("total_", bur_meas, "_", intervention_name, ".png")
  
  png(file.path(out_fig_path, barplot_fl_nm),
      width = 17,
      height = 7,
      units = "cm",
      pointsize = 12,
      res = 300)
  
  print(p)
  
  dev.off()
  
}
