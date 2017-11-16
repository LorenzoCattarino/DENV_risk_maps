library(dplyr)

source(file.path("R", "prepare_datasets", "average_up.r"))
source(file.path("R", "utility_functions.r"))


# ---------------------------------------- define parameters 


model_type <- "boot_model_20km_2"

scenario_id <- 3

base_info <- c("cell", "lat.grid", "long.grid", "population", "ADM_0", "ADM_1", "ADM_2")

P_a <- 0.7

out_pth <- file.path("output", 
                     "predictions_world", 
                     model_type, 
                     "means")

out_fl_nm <- paste0("C_inc_after_vacc_mean_all_squares_0_1667_deg_", scenario_id, ".rds")


# ---------------------------------------- load data 


covariates_vals <- readRDS(
  file.path("output",
            "env_variables",
            "all_squares_env_var_0_1667_deg.rds"))

foi <- readRDS(
  file.path("output", 
            "predictions_world", 
            model_type, 
            "means", 
            "FOI_mean_all_squares_0_1667_deg.rds"))

R0_vals <- readRDS(
  file.path("output", 
            "predictions_world", 
            model_type, 
            "means", 
            paste0("R0_r_mean_all_squares_0_1667_deg_", scenario_id, ".rds")))
  
look_up_table <- read.csv(
  file.path("data",
            "vaccine",
            "R0_to_prop_cases_averted_lookup.csv"),
  header = TRUE)


cases <- readRDS(
  file.path("output", 
            "predictions_world", 
            model_type, 
            "means", 
            paste0("C_num_mean_all_squares_0_1667_deg_", scenario_id, ".rds")))
  
  
# ---------------------------------------- change names to avoid confusion when joining tables 


names(R0_vals)[names(R0_vals)=="mean"] <- "R0"
R0_vals <- R0_vals[setdiff(names(R0_vals), c("sd", "lCI", "uCI"))]

names(cases)[names(cases)=="mean"] <- "C_num"
cases <- cases[setdiff(names(cases), c("sd", "lCI", "uCI"))]


# ---------------------------------------- start


R0_vals <- cbind(R0_vals, cases[, "C_num", drop = FALSE])

foi <- cbind(covariates_vals[, base_info], foi)
  
adm_foi <- average_up(
  foi, 
  c("ADM_0", "ADM_1"), 
  "mean")

adm_foi$a <- (-log(1 - P_a)) / (4 * adm_foi$mean)

adm_foi$a <- round(adm_foi$a)

adm_foi$a[adm_foi$a < 9] <- 9

adm_foi$a[adm_foi$a >= 9 & adm_foi$a <= 18] <- adm_foi$a[adm_foi$a >= 9 & adm_foi$a <= 18]

adm_foi$a[adm_foi$a > 18] <- -1

R0_vals <- left_join(R0_vals, adm_foi[, c("ADM_0", "ADM_1", "a")], by = c("ADM_0", "ADM_1"))

R0_vals$pCav <- 0

logical_subset <- R0_vals$a != -1

R0_vals_2 <- R0_vals[logical_subset, ]


# ---------------------------------------- fix the extreme R0 values in lookup_table


new_first_row <- cbind(R0 = 0, look_up_table[1, 2:18])
new_last_row <- cbind(R0 = 10, look_up_table[nrow(look_up_table), 2:18])

look_up_table_2 <- rbind(new_first_row, look_up_table, new_last_row)


# ---------------------------------------- convert to matrices


R0_vals_2 <- as.matrix(R0_vals_2)

look_up_table_2 <- as.matrix(look_up_table_2)


# ---------------------------------------- loop


for (i in seq_len(nrow(R0_vals_2))){
  
  a_i <- R0_vals_2[i, "a"]
  #cat("a =", a_i, "\n")
  
  R0_i <- R0_vals_2[i, "R0"]
    
  prop_cases_avert <- approx(look_up_table_2[, "R0"], look_up_table_2[, a_i], xout = R0_i)$y

  R0_vals_2[i, "pCav"] <- prop_cases_avert
  
}


# ---------------------------------------- 


R0_vals_3 <- rbind(R0_vals_2, R0_vals[!logical_subset, ])
  
C_num_net_vaccine <- (1 - R0_vals_3[, "pCav"]) * R0_vals_3[, "C_num"]

C_inc_net_vaccine <- (C_num_net_vaccine / R0_vals_3[, "population"]) * 1000

R0_vals_3 <- cbind(R0_vals_3, C_num_vacc = C_num_net_vaccine, mean = C_inc_net_vaccine)

write_out_rds(as.data.frame(R0_vals_3), out_pth, out_fl_nm)
