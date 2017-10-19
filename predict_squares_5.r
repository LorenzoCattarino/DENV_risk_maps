# Take the mean, sd and 95%CI across bootstrap samples, of number of 
# 1) total number of infections and cases, summed over all squares, AND:
# 2) total number of infections and cases, summed by country 

library(dplyr)
library(data.table)
library(ggplot2)

source(file.path("R", "utility_functions.r"))
source(file.path("R", "prepare_datasets", "calculate_mean_across_fits.r"))


# ---------------------------------------- define parameters


model_tp <- "boot_model_20km_2"

in_path <- file.path("output", "predictions_world", model_tp)

no_fits <- 200

var_to_sum <- as.character(seq_len(no_fits))

out_table_path <- file.path("output", "predictions_world", model_tp)
out_fig_path <- file.path("figures", "predictions_world", model_tp)

vars <- c("I", "C")

var_names <- c("infections", "cases")

no_scenarios <- 9


# ---------------------------------------- load data 


fct_c_2 <- read.csv(
  file.path(
    "output", 
    "predictions_world", 
    model_tp,
    "scenario_table.csv"),
  header = TRUE)

age_struct <- read.csv(
  file.path("output", 
            "datasets",
            "country_age_structure.csv"), 
  header = TRUE) 


# ---------------------------------------- pre processing


names(age_struct)[names(age_struct) == "ID_0"] <- "ADM_0" 


# ---------------------------------------- aggreaggating


for (j in seq_along(vars)){
  
  my_var <- vars[j]
  
  my_var_name <- var_names[j]
  
  root_name <- paste0(my_var, "_num_all_squares_0_1667_deg_")
  
  out_ls <- vector("list", no_scenarios)
  
  for (i in seq_len(no_scenarios)){
    
    dat <- readRDS(file.path(in_path, paste0(root_name, i, ".rds")))
    
    by_country <- dat %>% group_by(ADM_0)
    
    country_sums <- by_country %>% summarise_at(var_to_sum, "sum")
    
    ret <- average_boot_samples_dim2(country_sums)
    
    ret2 <- cbind(ADM_0 = country_sums$ADM_0, ret)
    
    ret3 <- merge(age_struct[, c("country", "ADM_0")], ret2, by = "ADM_0", all.x = FALSE)
    
    write_out_csv(ret3, 
                  out_table_path, 
                  paste0("number_", my_var_name, "_by_country_scenario_", i, ".csv"))
    
    ret4 <- colSums(dat[, var_to_sum])
    
    ret5 <- average_boot_samples_dim1(ret4)
    
    names(ret5) <- paste(names(ret5), my_var_name, sep="_")
    
    out_ls[[i]] <- ret5
    
  }
  
  fct_c_2 <- cbind(fct_c_2, do.call("rbind", out_ls))
  
}


# ---------------------------------------- plotting


summary_table <- fct_c_2

summary_table$scaling_factor <- factor(summary_table$scaling_factor,
                                       levels = c(1,0.7,0.3),
                                       labels = c(1,0.7,0.3))

phi_factor_levels <- c("Primary and secondary", "4 Equal", "4 Equal (sym = 2x asym)")

summary_table$phi_set_id <- factor(summary_table$phi_set_id, 
                                   levels = c(1, 3, 4), 
                                   labels = phi_factor_levels)

summary_table_long <- reshape(summary_table, 
                              varying = 8:ncol(summary_table), 
                              sep = "_", 
                              direction = 'long')

summary_table_long$time <- factor(summary_table_long$time,
                                  levels = c("infections", "cases"),
                                  labels = c("Infections", "Cases"))

total_burden_baseline <- subset(summary_table_long, scaling_factor == 1)

write_out_csv(total_burden_baseline, out_table_path, "total_burden.csv")

y_values <- pretty(summary_table_long$mean, 8)

ggplot(summary_table_long, aes(time, mean, fill = scaling_factor, ymin = lCI, ymax = uCI)) + 
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(width = .25, position = position_dodge(.9)) +
  scale_fill_manual(values = c("lightskyblue1", "lightskyblue3", "lightskyblue4"),
                    labels = c("0%", "50%", "70%"),
                    guide = guide_legend(title = "R0 reduction", 
                                         keywidth = 2, 
                                         keyheight = 2)) +
  facet_grid(. ~ phi_set_id) +
  xlab(NULL) +
  scale_y_continuous(NULL, 
                     breaks = y_values, 
                     labels = format(y_values/1000000), 
                     limits = c(min(y_values), max(y_values))) +
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
        strip.text.x = element_text(size = 12))

dir.create(out_fig_path, FALSE, TRUE)

ggsave(file.path(out_fig_path, "Wolbachia_effect_on_infections_&_cases_barplot.png"),
       width = 12, 
       height = 4, 
       units = "in")

  
# # ---------------------------------------- combine all scenario results 
# 
# 
# all_bur_scenarios <- R0_and_burden$results()
# 
# all_bur_scenarios <- do.call("cbind", all_bur_scenarios)
# 
# out <- cbind(all_sqr_mean_foi, all_bur_scenarios)
# 
# 
# # ---------------------------------------- post processing 
# 
# 
# zero_logic <- out$FOI_1 == 0
# 
# out_mz <- out[!zero_logic, ] 
# 
# out_mz$p9 <- 100 * (1 - exp(-36 * out_mz$FOI_1))
# 
# names(out_mz)[names(out_mz) == "mean_pred"] <- "R_0"
# 
# write_out_rds(out_mz,
#               out_path, 
#               "R0_and_burden_all_combs.rds")
# 
# 
# # ---------------------------------------- summarize burden by country 
# 
# 
# n_combs <- nrow(fct_c_2)
# 
# var_to_sum <- c("population",
#                 paste0("I_num_", seq_len(n_combs)),
#                 paste0("C_num_", seq_len(n_combs)))
# 
# by_country <- out_mz %>% group_by(country)
# 
# inf_cas_sums <- by_country %>% summarise_each("sum", one_of(var_to_sum))
# 
# num_Inf_and_C_by_c <- as.data.frame(inf_cas_sums)
# 
# write.csv(num_Inf_and_C_by_c, 
#           file.path(out_path, "total_infec_and_cases_by_country.csv"),
#           row.names = FALSE)
# 
# 
# # ---------------------------------------- plot number and incidence of infections and cases, for each scenario  
# 
# 
# summed_vars <- colSums(num_Inf_and_C_by_c[, var_to_sum])
# 
# resh_res <- setNames(data.frame(matrix(summed_vars[2:length(var_to_sum)], ncol = 2)),
#                      nm = c("Infections", "Cases"))
# 
# resh_res$Incidence_of_infections <- (resh_res$Infections / summed_vars[1]) * 1000
# 
# resh_res$Incidence_of_cases <- (resh_res$Cases / summed_vars[1]) * 1000
# 
# summary_table <- cbind(fct_c_2, resh_res)
