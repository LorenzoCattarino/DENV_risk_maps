# Calculates
# for each model fit, 
# R0-Wolbachia effect assumption combinations and 
# 20 km square:
#
# 1) FOI or (R0) corresponding to the response originally fitted
# 2) number of infections
# 3) number of cases
# 4) number of hospitalized cases 

options(didehpc.cluster = "fi--didemrchnb")

CLUSTER <- FALSE

my_resources <- c(
  file.path("R", "burden_and_interventions", "wrapper_to_multi_factor_R0_and_burden.R"),
  file.path("R", "burden_and_interventions", "wrapper_to_replicate_R0_and_burden.R"),  
  file.path("R", "burden_and_interventions", "wrapper_to_R0_and_burden.R"),
  file.path("R", "burden_and_interventions", "functions_for_calculating_burden.R"),
  file.path("R", "prepare_datasets", "functions_for_calculating_R0.R"),
  file.path("R", "utility_functions.R"))

my_pkgs <- c("data.table", "dplyr")

context::context_log_start()
ctx <- context::context_save(path = "context",
                             packages = my_pkgs,
                             sources = my_resources)

context::context_load(ctx)
context::parallel_cluster_start(7, ctx)


# define parameters ----------------------------------------------------------- 


parameters <- list(
  id = 16,
  grid_size = 5,
  no_predictors = 23,
  resample_grid_size = 20,
  no_samples = 200,
  gamma_1 = 0.45,
  rho = 0.85,
  gamma_3 = 0.15,
  Q_1 = 0.04,
  Q_3 = 0.04,
  Q_4 = 0.04,
  Q_2 = 0.1,
  fit_type = "boot") 

parallel_2 <- TRUE

sf_vals <- c(1, 0.7, 0.3)
phi_set_id_tag <- "phi_set_id"

R0_assumptions <- list(
  v1 = c(1, 1, 0, 0),
  v2 = c(1, 1, 1, 1),  
  v3 = calculate_infectiousness_wgts_for_sym_asym_assumption(parameters))

FOI_values <- seq(0, 0.2, by = 0.0002)

prob_fun <- list("calculate_primary_infection_prob",
                 "calculate_secondary_infection_prob",
                 "calculate_tertiary_infection_prob",
                 "calculate_quaternary_infection_prob")

base_info <- c("population", "ID_0", "ID_1")


# define variables ------------------------------------------------------------


model_type <- paste0("model_", parameters$id)

out_path <- file.path("output", 
                      "predictions_world", 
                      "bootstrap_models", 
                      model_type,
                      "adm_1")

no_R0_assumptions <- length(R0_assumptions)


# load data ------------------------------------------------------------------- 


bootstrap_experiments <- read.csv(file.path("output", 
                                            "EM_algorithm", 
                                            "bootstrap_models", 
                                            "boostrap_fit_experiments_uni.csv"),
                                  stringsAsFactors = FALSE)

sqr_preds <- readRDS(file.path("output", 
                               "predictions_world",
                               "bootstrap_models",
                               model_type,
                               "adm_1",
                               "response.rds"))

age_struct <- read.csv(file.path("output", 
                                 "datasets",
                                 "country_age_structure.csv"), 
                       header = TRUE) 


# pre processing --------------------------------------------------------------


fit_var <- bootstrap_experiments[bootstrap_experiments$exp_id == parameters$id, "var"]

age_band_tgs <- grep("band", names(age_struct), value = TRUE)
age_band_bnds <- get_age_band_bounds(age_band_tgs)
age_band_L_bounds <- age_band_bnds[, 1]
age_band_U_bounds <- age_band_bnds[, 2] + 1

age_struct$age_id <- seq_len(nrow(age_struct))

# keep only the predictions for which there is age data available
sqr_preds <- inner_join(age_struct[, c("age_id", "ID_0")],
                        sqr_preds, 
                        by = "ID_0")


# create table of scenarios --------------------------------------------------- 


phi_set_id <- seq_len(no_R0_assumptions)

phi_combs <- setNames(data.frame(phi_set_id, do.call("rbind", R0_assumptions)),
                      nm = c(phi_set_id_tag, paste0("phi", seq_len(4))))

fct_c <- setNames(expand.grid(phi_set_id, sf_vals),
                  nm = c(phi_set_id_tag, "scaling_factor"))

fct_c <- cbind(id = seq_len(nrow(fct_c)), fct_c)

assumption <- as.numeric(unlist(strsplit(fit_var, "_"))[2])

fct_c <- subset(fct_c, phi_set_id == assumption)  

fct_c_2 <- left_join(fct_c, phi_combs, by = phi_set_id_tag)

write_out_csv(fct_c_2, out_path, "scenario_table_wolbachia.csv")

fctr_combs <- df_to_list(fct_c_2, use_names = TRUE)


# create FOI -> Inf and FOI -> C lookup tables -------------------------------- 


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
  
  FOI_to_Inf_list <- lapply(Infection_values, cbind_FOI_to_lookup, FOI_values)
  
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
                      parms = parameters,
                      parallel = TRUE)
  
  FOI_to_C_list <- lapply(Case_values, cbind_FOI_to_lookup, FOI_values)
  
  saveRDS(FOI_to_C_list, file.path(out_path, "FOI_to_C_lookup_tables.rds"))
  
} else {
  
  FOI_to_C_list <- readRDS(file.path(out_path, "FOI_to_C_lookup_tables.rds"))
  
}

if(!file.exists(file.path(out_path, "FOI_to_HC_lookup_tables.rds"))){
  
  HCase_values <- loop(seq_len(nrow(age_struct)), 
                       wrapper_to_lookup,
                       age_struct = age_struct, 
                       tags = age_band_tgs, 
                       FOI_values = FOI_values, 
                       my_fun = calculate_hosp_cases,
                       prob_fun = prob_fun,
                       age_band_lower_bounds = age_band_L_bounds,
                       age_band_upper_bounds = age_band_U_bounds,
                       parms = parameters,
                       parallel = TRUE)
  
  FOI_to_HC_list <- lapply(HCase_values, cbind_FOI_to_lookup, FOI_values)
  
  saveRDS(FOI_to_HC_list, file.path(out_path, "FOI_to_HC_lookup_tables.rds"))
  
} else {
  
  FOI_to_HC_list <- readRDS(file.path(out_path, "FOI_to_HC_lookup_tables.rds"))
  
}


# convert to matrix -----------------------------------------------------------


sqr_preds <- as.matrix(sqr_preds)

# sqr_preds <- sqr_preds[285510:285520,]

if (CLUSTER) {
  
  config <- didehpc::didehpc_config(template = "20Core")
  obj <- didehpc::queue_didehpc(ctx, config = config)
  
}


# submit one job --------------------------------------------------------------


# burden <- obj$enqueue(
#   wrapper_to_multi_factor_R0_and_burden(fctr_combs[[1]],
#                                         foi_data = sqr_preds,
#                                         age_data = age_struct,
#                                         age_band_tags = age_band_tgs,
#                                         age_band_lower_bounds = age_band_L_bounds,
#                                         age_band_upper_bounds = age_band_U_bounds,
#                                         parallel_2 = parallel_2,
#                                         FOI_values = FOI_values,
#                                         FOI_to_Inf_list = FOI_to_Inf_list,
#                                         FOI_to_C_list = FOI_to_C_list,
#                                         FOI_to_HC_list = FOI_to_HC_list,
#                                         prob_fun = prob_fun,
#                                         parms = parameters,
#                                         base_info = base_info,
#                                         out_path = out_path))


# submit a bundle -------------------------------------------------------------


if (CLUSTER) {
  
  burden <- queuer::qlapply(fctr_combs,
                            wrapper_to_multi_factor_R0_and_burden,
                            obj,
                            foi_data = sqr_preds,
                            age_data = age_struct,
                            age_band_tags = age_band_tgs,
                            age_band_lower_bounds = age_band_L_bounds,
                            age_band_upper_bounds = age_band_U_bounds,
                            parallel_2 = parallel_2,
                            FOI_values = FOI_values,
                            FOI_to_Inf_list = FOI_to_Inf_list,
                            FOI_to_C_list = FOI_to_C_list,
                            FOI_to_HC_list = FOI_to_HC_list,
                            prob_fun = prob_fun,
                            parms = parameters,
                            base_info = base_info,
                            out_path = out_path)
  
} else {
  
  burden <- loop(fctr_combs, 
                 wrapper_to_multi_factor_R0_and_burden,
                 foi_data = sqr_preds,
                 age_data = age_struct,
                 age_band_tags = age_band_tgs,
                 age_band_lower_bounds = age_band_L_bounds,
                 age_band_upper_bounds = age_band_U_bounds,
                 parallel_2 = parallel_2,
                 FOI_values = FOI_values,
                 FOI_to_Inf_list = FOI_to_Inf_list,
                 FOI_to_C_list = FOI_to_C_list,
                 FOI_to_HC_list = FOI_to_HC_list,
                 prob_fun = prob_fun,
                 parms = parameters,
                 base_info = base_info,
                 out_path = out_path,
                 parallel = FALSE)
  
}

if (!CLUSTER){
  
  context::parallel_cluster_stop()
  
}
