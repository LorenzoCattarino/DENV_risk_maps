# Calculates
# for each R0-Wolbachia effect assumption combinations and 
# 20 km square:
#
# 1) R0 or (FOI) 
# 2) corresponding FOI or (R0) (depending on the response originally fitted)
# 3) number of infections
# 4) number of cases
# 5) number of hospitalized cases  

my_resources <- c(
  file.path("R", "burden_and_interventions", "functions_for_calculating_burden.R"),
  file.path("R", "burden_and_interventions", "wrapper_to_multi_factor_R0_and_burden.R"),
  file.path("R", "burden_and_interventions", "wrapper_to_replicate_R0_and_burden.R"),
  file.path("R", "prepare_datasets", "functions_for_calculating_R0.R"),
  file.path("R", "utility_functions.R"),
  file.path("R", "create_parameter_list.R"))

my_pkgs <- c("data.table", "dplyr")

context::context_log_start()
ctx <- context::context_save(path = "context",
                             packages = my_pkgs,
                             sources = my_resources)
context::context_load(ctx)
context::parallel_cluster_start(8, ctx)


# define parameters ----------------------------------------------------------- 


extra_prms <- list(id = 14,
                   dependent_variable = "Z",
                   fit_type = "best",
                   parallel_2 = TRUE,
                   base_info = c("cell", 
                                 "latitude", 
                                 "longitude", 
                                 "population", 
                                 "ID_0", 
                                 "ID_1", 
                                 "ID_2"))   


# define variables ------------------------------------------------------------


parameters <- create_parameter_list(extra_params = extra_prms)

model_type <- paste0("model_", parameters$id)

lookup_path <- file.path("output", 
                         "predictions_world")

out_path <- file.path("output", 
                      "predictions_world", 
                      "best_fit_models", 
                      model_type)

no_R0_assumptions <- 4

sf_vals <- parameters$sf_vals


# load data ------------------------------------------------------------------- 


FOI_to_Inf_list <- readRDS(file.path(lookup_path, "FOI_to_I_lookup_tables.rds"))

FOI_to_C_list <- readRDS(file.path(lookup_path, "FOI_to_C_lookup_tables.rds"))

FOI_to_HC_list <- readRDS(file.path(lookup_path, "FOI_to_HC_lookup_tables.rds"))

FOI_to_R0_1_list <- readRDS(file.path(lookup_path, "FOI_to_R0_1_lookup_tables.rds"))

FOI_to_R0_2_list <- readRDS(file.path(lookup_path, "FOI_to_R0_2_lookup_tables.rds"))

FOI_to_R0_3_list <- readRDS(file.path(lookup_path, "FOI_to_R0_3_lookup_tables.rds"))

FOI_to_C_list_fixed <- readRDS(file.path(lookup_path, "FOI_to_C_lookup_tables_fixed_params.rds"))

FOI_to_HC_list_fixed <- readRDS(file.path(lookup_path, "FOI_to_HC_lookup_tables_fixed_params.rds"))

sqr_preds <- readRDS(file.path("output", 
                               "predictions_world",
                               "best_fit_models",
                               model_type,
                               "response.rds"))

sqr_covariates <- readRDS(file.path("output", 
                                    "env_variables", 
                                    "all_squares_env_var_0_1667_deg.rds"))

age_struct <- read.csv(file.path("output", 
                                 "datasets",
                                 "country_age_structure.csv"), 
                       header = TRUE) 


# pre processing --------------------------------------------------------------


fit_var <- parameters$dependent_variable

task_b_name <- paste0("burden_", fit_var)

age_band_tgs <- grep("band", names(age_struct), value = TRUE)
age_band_bnds <- get_age_band_bounds(age_band_tgs)
age_band_L_bounds <- age_band_bnds[, 1]
age_band_U_bounds <- age_band_bnds[, 2] + 1

age_struct$age_id <- seq_len(nrow(age_struct))

# keep onle the FOI for which there is age data available
sqr_preds <- inner_join(age_struct[, c("age_id", "ID_0")],
                        sqr_preds, 
                        by = "ID_0")

sqr_preds <- as.matrix(sqr_preds)


# create table of scenarios ---------------------------------------------------  


phi_set_id <- seq_len(no_R0_assumptions)

fct_c <- setNames(expand.grid(phi_set_id, sf_vals),
                  nm = c("phi_set_id", "scaling_factor"))

fct_c <- cbind(id = seq_len(nrow(fct_c)), fct_c)

if(fit_var == "FOI" | fit_var == "Z"){
  
  assumption <- 4 
  
} else {
  
  assumption <- as.numeric(unlist(strsplit(fit_var, "_"))[2])
  
}

fct_c <- subset(fct_c, phi_set_id == assumption)

write_out_csv(fct_c, out_path, "scenario_table_wolbachia.csv")

fctr_combs <- df_to_list(fct_c, use_names = TRUE)


# submit job ------------------------------------------------------------------ 


R0_and_burden <- loop(fctr_combs[1],
                      wrapper_to_multi_factor_R0_and_burden,
                      foi_data = sqr_preds,
                      age_data = age_struct,
                      age_band_tags = age_band_tgs,
                      age_band_lower_bounds = age_band_L_bounds,
                      age_band_upper_bounds = age_band_U_bounds,
                      FOI_to_Inf_list = FOI_to_Inf_list,
                      FOI_to_C_list = FOI_to_C_list,
                      FOI_to_C_list_fixed = FOI_to_C_list_fixed,
                      FOI_to_HC_list = FOI_to_HC_list,
                      FOI_to_HC_list_fixed = FOI_to_HC_list_fixed,
                      FOI_to_R0_1_list = FOI_to_R0_1_list,
                      FOI_to_R0_2_list = FOI_to_R0_2_list,
                      FOI_to_R0_3_list = FOI_to_R0_3_list,
                      parms = parameters,
                      out_path = out_path,
                      var_to_fit = fit_var,
                      parallel = FALSE)
