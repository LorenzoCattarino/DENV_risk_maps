# Calculates, for each model fit and R0-Wolbachia effect assumption combinations
# R0 values and burden, for different foi value (squares).

# Takes the mean and confidence intervals, across models,  
# of different measured variables (foi, R0, burden), for each tile. 


# ----------------------------------------


options(didehpc.cluster = "fi--didemrchnb")

my_resources <- c(
  file.path("R", "utility_functions.r"),
  file.path("R", "burden_and_interventions", "wrapper_to_multi_factor_R0_and_burden.r"),
  file.path("R", "burden_and_interventions", "wrapper_to_replicate_R0_and_burden.r"),  
  file.path("R", "burden_and_interventions", "wrapper_to_R0_and_burden.r"),
  file.path("R", "burden_and_interventions", "functions_for_calculating_burden.r"),
  file.path("R", "prepare_datasets", "functions_for_calculating_R0.r"))

my_pkgs <- c("data.table", "dplyr", "reshape2", "ggplot2")

context::context_log_start()
ctx <- context::context_save(path = "context",
                             packages = my_pkgs,
                             sources = my_resources)

context::context_load(ctx)
context::parallel_cluster_start(8, ctx)

CLUSTER <- TRUE


# ---------------------------------------- define parameters


model_tp <- "boot_model_20km_2" 

no_fits <- 200

var_names <- c("FOI_r", "R0_r", "I_num", "C_num", "I_inc", "C_inc")

phi_set_id <- c(1, 3, 4)
sf_vals <- c(1, 0.7, 0.3)
phi_set_id_tag <- "phi_set_id"
gamma_1 <- 0.45
rho <- 0.85
gamma_3 <- 0.15

v1 <- c(1, 1, 0, 0) # Up to 2 infections
v2 <- c(1, 1, 1, 0) # Up to 3 infections
v3 <- c(1, 1, 1, 1) # Up to 4 infections 

FOI_values <- seq(0, 0.1, by = 0.0002)

prob_fun <- list("calculate_primary_infection_prob",
                 "calculate_secondary_infection_prob",
                 "calculate_tertiary_infection_prob",
                 "calculate_quaternary_infection_prob")

out_path <- file.path("output", "predictions_world", model_tp)

base_info <- c("cell", "lat.grid", "long.grid", "population", "ADM_0", "ADM_1", "ADM_2")


# ---------------------------------------- load data


# all_sqr_mean_foi <- readRDS(
#   file.path(
#     "output", 
#     "predictions_world",
#     model_tp,
#     "all_squares_mean_foi_0_1667_deg.rds"))

all_sqr_foi <- readRDS(
  file.path(
    "output", 
    "predictions_world",
    model_tp,
    "FOI_all_squares_0_1667_deg.rds"))

all_sqr_covariates <- readRDS(
  file.path(
    "output", 
    "env_variables", 
    "all_squares_env_var_0_1667_deg.rds"))

age_struct <- read.csv(
  file.path("output", 
            "datasets",
            "country_age_structure.csv"), 
  header = TRUE) 


# ---------------------------------------- 


age_struct$age_id <- seq_len(nrow(age_struct))

names(age_struct)[names(age_struct) == "ID_0"] <- "ADM_0"

all_sqr_foi <- cbind(all_sqr_covariates[, base_info], all_sqr_foi)


# ---------------------------------------- keep onle the FOI for which there is age data available


all_sqr_foi <- inner_join(
  age_struct[, c("age_id", "country", "ADM_0")],
  all_sqr_foi, 
  by = "ADM_0")


# ---------------------------------------- create table of scenarios 


v4 <- calculate_infectiousness_wgts_for_sym_asym_assumption(gamma_1, rho, gamma_3)
phi_combs <- setNames(data.frame(seq_len(4), rbind(v1, v2, v3, v4)),
                      nm = c(phi_set_id_tag, "phi1", "phi2", "phi3", "phi4"))

fct_c <- setNames(expand.grid(phi_set_id, sf_vals),
                  nm = c(phi_set_id_tag, "scaling_factor"))

fct_c <- cbind(id = seq_len(nrow(fct_c)), fct_c)

fct_c_2 <- left_join(fct_c, phi_combs, by = phi_set_id_tag)
write.csv(fct_c_2, 
          file.path("output", "predictions_world", model_tp, "scenario_table.csv"), 
          row.names = FALSE)

fctr_combs <- df_to_list(fct_c_2, use_names = TRUE)


# ----------------------------------------


age_band_tgs <- grep("band", names(age_struct), value = TRUE)
age_band_bnds <- get_age_band_bounds(age_band_tgs)
age_band_L_bounds <- age_band_bnds[, 1]
age_band_U_bounds <- age_band_bnds[, 2] + 1


# ---------------------------------------- create FOI -> Inf and FOI -> C lookup tables 


if(!file.exists(file.path(out_path, "FOI_to_I_lookup_tables.rds"))){
  
  Infection_values <- loop(seq_len(nrow(age_struct)), 
                           wrapper_to_lookup,
                           age_struct = age_struct, 
                           tags = age_band_tgs, 
                           FOI_values = FOI_values, 
                           my_fun = calculate_infections,
                           prob_fun = prob_fun,
                           age_band_lower_bounds = age_band_L_bounds,
                           age_band_upper_bounds = age_band_U_bounds,
                           parallel = TRUE)
  
  FOI_to_Inf_list <- lapply(Infection_values, function(i) cbind(x = FOI_values, y = i))
  
  saveRDS(FOI_to_Inf_list, file.path(out_path, "FOI_to_I_lookup_tables.rds"))
  
} else {
  
  FOI_to_Inf_list <- readRDS(file.path(out_path, "FOI_to_I_lookup_tables.rds"))
  
}

if(!file.exists(file.path(out_path, "FOI_to_C_lookup_tables.rds"))){
  
  Case_values <- loop(seq_len(nrow(age_struct)), 
                      wrapper_to_lookup,
                      age_struct = age_struct, 
                      tags = age_band_tgs, 
                      FOI_values = FOI_values, 
                      my_fun = calculate_cases,
                      prob_fun = prob_fun,
                      age_band_lower_bounds = age_band_L_bounds,
                      age_band_upper_bounds = age_band_U_bounds,
                      rho = rho, 
                      gamma_1 = gamma_1, 
                      gamma_3 = gamma_3,
                      parallel = TRUE)
  
  FOI_to_C_list <- lapply(Case_values, function(i) cbind(x = FOI_values, y = i))
  
  saveRDS(FOI_to_C_list, file.path(out_path, "FOI_to_C_lookup_tables.rds"))
  
} else{
  
  FOI_to_C_list <- readRDS(file.path(out_path, "FOI_to_C_lookup_tables.rds"))
  
}

# ------------------------------------------ submit jobs 


if (CLUSTER) {
  
  config <- didehpc::didehpc_config(template = "12and16Core")
  obj <- didehpc::queue_didehpc(ctx, config = config)
  
}

if (CLUSTER) {
  
  R0_and_burden <- queuer::qlapply(
    fctr_combs,
    wrapper_to_multi_factor_R0_and_burden,
    obj,
    foi_data = all_sqr_foi, 
    age_data = age_struct,
    age_band_tags = age_band_tgs,
    age_band_lower_bounds = age_band_L_bounds,
    age_band_upper_bounds = age_band_U_bounds,
    parallel_2 = TRUE,    
    var_names = var_names, 
    FOI_values = FOI_values,
    FOI_to_Inf_list = FOI_to_Inf_list,
    FOI_to_C_list = FOI_to_C_list,
    prob_fun = prob_fun,
    no_fits = no_fits,
    out_path = out_path,
    base_info = base_info,
    reverse = FALSE)
  
} else {
  
  R0_and_burden <- loop(
    fctr_combs[1],
    wrapper_to_multi_factor_R0_and_burden,
    foi_data = all_sqr_foi, 
    age_data = age_struct,
    age_band_tags = age_band_tgs,
    age_band_lower_bounds = age_band_L_bounds,
    age_band_upper_bounds = age_band_U_bounds,
    parallel_2 = TRUE,    
    var_names = var_names, 
    FOI_values = FOI_values,
    FOI_to_Inf_list = FOI_to_Inf_list,
    FOI_to_C_list = FOI_to_C_list,
    prob_fun = prob_fun,
    no_fits = no_fits,
    out_path = out_path,
    base_info = base_info,
    reverse = FALSE,
    parallel = FALSE)
  
}

context::parallel_cluster_stop()
