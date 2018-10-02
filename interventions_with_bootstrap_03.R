# For wolbachia impact

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
  id = c(14, 15, 16),
  no_samples = 200,
  burden_measures = c("infections", "cases", "hosp"),
  desired_n_int = c(8, 8, 5),
  baseline_scenario_ids = c(1, 2, 3))   

intervention_name <- "wolbachia"

treatment_name <- "scaling_factor"


# define variables ------------------------------------------------------------


baseline_scenario_ids <- parameters$baseline_scenario_ids 
  
desired_n_int <- parameters$desired_n_int

burden_measures <- parameters$burden_measures

vars <- toupper(substr(burden_measures, 1, 1))

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
                            intervention_name)

out_fig_path <- file.path("figures", 
                          "predictions_world", 
                          "bootstrap_models")

fct_comb_fl_nm <- paste0("scenario_table_", intervention_name, ".csv")


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


for (j in seq_along(vars)){                                 # loop over burden measures 
  
  out_ls <- vector("list", length(model_type))
  out_ls_2 <- vector("list", length(model_type))
    
  my_var_name <- burden_measures[j]
  cat("burden measure =", my_var_name, "\n")
  
  my_var <- vars[j]
  
  root_name <- paste0(my_var, "_num_", intervention_name)
  
  for (k in seq_along(model_type)){                         # loop over R0 assumptions

    cat("R0 assumption =", k, "\n")
    
    # index <- k + (length(in_path)*(j-1))
    
    my_in_path <- in_path[k]
    my_out_path <- out_table_path[k]
    
    my_fct_comb <- fct_comb_ls[[k]]
    
    fi <- list.files(my_in_path, 
                     pattern = paste0("^", root_name),
                     full.names = TRUE)
    
    dat <- lapply(fi, readRDS)
    
    small_out_ls <- vector("list", length(dat))
    small_out_ls_2 <- vector("list", length(dat))
      
    baseline_id <- baseline_scenario_ids[k]
    
    baseline <- readRDS(file.path(my_in_path, paste0(root_name, "_", baseline_id, ".rds"))) 
    
    for (i in seq_along(dat)){                              # loop over treatments
      
      scenario_id <- my_fct_comb[i, "id"]
      cat("scenario table id =", scenario_id, "\n")
      
      one_dat <- as.data.frame(dat[[i]])
      one_dat <- left_join(one_dat, age_struct[, c("continent", "region", "country", "ID_0")])
      
      by_country <- one_dat %>% group_by(ID_0)
      country_sums <- by_country %>% summarise_at(var_to_sum, "sum")
      ret <- average_boot_samples_dim2(country_sums[, var_to_sum])
      ret2 <- cbind(ID_0 = country_sums$ID_0, ret)
      ret3 <- merge(age_struct[, c("country", "ID_0")], ret2, by = "ID_0", all.x = FALSE)
      write_out_csv(ret3, 
                    my_out_path, 
                    paste0(my_var_name, "_by_country_", scenario_id, ".csv"))
      
      by_continent <- one_dat %>% group_by(continent)
      continent_sums <- by_continent %>% summarise_at(var_to_sum, "sum")       
      ret <- average_boot_samples_dim2(continent_sums[, var_to_sum])
      ret2 <- cbind(continent = continent_sums$continent, ret)  
      write_out_csv(ret2, 
                    my_out_path, 
                    paste0(my_var_name, "_by_continent_", scenario_id, ".csv"))
      
      by_region <- one_dat %>% group_by(region)
      region_sums <- by_region %>% summarise_at(var_to_sum, "sum")
      ret <- average_boot_samples_dim2(region_sums[, var_to_sum])
      ret2 <- cbind(region = region_sums$region, ret)  
      write_out_csv(ret2, 
                    my_out_path, 
                    paste0(my_var_name, "_by_region_", scenario_id, ".csv"))
      
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
  
  names(summary_table)[names(summary_table) == treatment_name] <- "treatment"
  
  treatment_levels <- unique(summary_table$treatment)
  
  summary_table[, "treatment"] <- factor(summary_table[, "treatment"],
                                         levels = treatment_levels,
                                         labels = treatment_levels)
  
  phi_factor_levels <- c("2S", "4S", "4S(sym = 2x asym)")
  
  summary_table$phi_set_id <- factor(summary_table$phi_set_id, 
                                     levels = c(1, 2, 3), 
                                     labels = phi_factor_levels)

  summary_table_2 <- do.call("rbind", out_ls_2)  
  
  names(summary_table_2)[names(summary_table_2) == treatment_name] <- "treatment"
  
  treatment_levels <- unique(summary_table_2$treatment)
  
  summary_table_2[, "treatment"] <- factor(summary_table_2[, "treatment"],
                                         levels = treatment_levels,
                                         labels = treatment_levels)
  
  phi_factor_levels <- c("2S", "4S", "4S(sym = 2x asym)")
  
  summary_table_2$phi_set_id <- factor(summary_table_2$phi_set_id, 
                                     levels = c(1, 2, 3), 
                                     labels = phi_factor_levels)
  
  summary_tab_fl_nm <- paste0("prop_change_", my_var_name, "_", intervention_name, ".csv")
    
  write_out_csv(summary_table_2, file.path("output", 
                                           "predictions_world", 
                                           "bootstrap_models"), 
                summary_tab_fl_nm)
  
  
  # save table of baseline burden ---------------------------------------------
  
  
  if(intervention_name == "wolbachia"){
    
    total_burden_baseline <- subset(summary_table, treatment == 1)
    
    basel_burden_fl_nm <- paste0("baseline_total_", my_var_name, "_", intervention_name, ".csv")
    
    write_out_csv(total_burden_baseline, file.path("output", 
                                                   "predictions_world", 
                                                   "bootstrap_models"), 
                  basel_burden_fl_nm)
    
  }
  
  
  # plotting ------------------------------------------------------------------


  y_values <- pretty(summary_table$mean, desired_n_int[j])
  max_y_value <- max(summary_table$uCI)

  p <- ggplot(summary_table, aes(treatment, mean, fill = treatment, ymin = lCI, ymax = uCI)) +
    geom_bar(stat = "identity", position = "dodge", width = 1) +
    geom_errorbar(width = .25, position = position_dodge(.9)) +
    facet_grid(. ~ phi_set_id) +
    scale_fill_manual(values = c("lightskyblue1", "lightskyblue3", "lightskyblue4"),
                      labels = c("0%", "50%", "70%"),
                      guide = guide_legend(title = expression('R'['0']*' reduction'),
                                           keywidth = 2,
                                           keyheight = 2)) +
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

  barplot_fl_nm <- paste0("total_", my_var_name, "_", intervention_name, ".png")

  png(file.path(out_fig_path, barplot_fl_nm),
      width = 17,
      height = 7,
      units = "cm",
      pointsize = 12,
      res = 300)

  print(p)

  dev.off()
  
}
