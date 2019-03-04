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
  file.path("R", "utility_functions.R"),
  file.path("R", "create_parameter_list.R"))

my_pkgs <- c("data.table", "dplyr")

context::context_log_start()
ctx <- context::context_save(path = "context",
                             packages = my_pkgs,
                             sources = my_resources)


# define extra parameters -----------------------------------------------------


extra_prms <- list(id = 24,
                   fit_type = "boot",
                   parallel_2 = TRUE,
                   phi_set_id_tag = "phi_set_id",
                   base_info = c("population", 
                                 "ID_0", 
                                 "ID_1"))


# are you using the cluster? --------------------------------------------------


if (CLUSTER) {
  
  config <- didehpc::didehpc_config(template = "16Core")
  obj <- didehpc::queue_didehpc(ctx, config = config)
  
} else {
  
  context::context_load(ctx)
  context::parallel_cluster_start(7, ctx)
  
}


# define variables ------------------------------------------------------------


parameters <- create_parameter_list(extra_params = extra_prms)

model_type <- paste0("model_", parameters$id)

lookup_path <- file.path("output", 
                         "predictions_world", 
                         "bootstrap_models")

out_path <- file.path("output", 
                      "predictions_world", 
                      "bootstrap_models", 
                      model_type,
                      "adm_1")

no_R0_assumptions <- 3

sf_vals <- parameters$sf_vals

phi_set_id_tag <- parameters$phi_set_id_tag


# load data ------------------------------------------------------------------- 


FOI_to_Inf_list <- readRDS(file.path(lookup_path, "FOI_to_I_lookup_tables.rds"))

FOI_to_C_list <- readRDS(file.path(lookup_path, "FOI_to_C_lookup_tables.rds"))

FOI_to_HC_list <- readRDS(file.path(lookup_path, "FOI_to_HC_lookup_tables.rds"))

FOI_to_R0_1_list <- readRDS(file.path(lookup_path, "FOI_to_R0_1_lookup_tables.rds"))

FOI_to_R0_2_list <- readRDS(file.path(lookup_path, "FOI_to_R0_2_lookup_tables.rds"))

FOI_to_R0_3_list <- readRDS(file.path(lookup_path, "FOI_to_R0_3_lookup_tables.rds"))

FOI_to_C_list_fixed <- readRDS(file.path(lookup_path, "FOI_to_C_lookup_tables_fixed_params.rds"))

FOI_to_HC_list_fixed <- readRDS(file.path(lookup_path, "FOI_to_HC_lookup_tables_fixed_params.rds"))

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

sqr_preds <- as.matrix(sqr_preds)

# sqr_preds <- sqr_preds[285510:285520,]


# create table of scenarios --------------------------------------------------- 


phi_set_id <- seq_len(no_R0_assumptions)

fct_c <- setNames(expand.grid(phi_set_id, sf_vals),
                  nm = c(phi_set_id_tag, "scaling_factor"))

fct_c <- cbind(id = seq_len(nrow(fct_c)), fct_c)

assumption <- as.numeric(unlist(strsplit(fit_var, "_"))[2])

fct_c <- subset(fct_c, phi_set_id == assumption)  

write_out_csv(fct_c, out_path, "scenario_table_wolbachia.csv")

fctr_combs <- df_to_list(fct_c, use_names = TRUE)


# submit one job --------------------------------------------------------------


# burden <- obj$enqueue(
#   wrapper_to_multi_factor_R0_and_burden(fctr_combs[[1]],
#                                         foi_data = sqr_preds,
#                                         age_data = age_struct,
#                                         age_band_tags = age_band_tgs,
#                                         age_band_lower_bounds = age_band_L_bounds,
#                                         age_band_upper_bounds = age_band_U_bounds,
#                                         FOI_to_Inf_list = FOI_to_Inf_list,
#                                         FOI_to_C_list = FOI_to_C_list,
#                                         FOI_to_C_list_fixed = FOI_to_C_list_fixed,
#                                         FOI_to_HC_list = FOI_to_HC_list,
#                                         FOI_to_HC_list_fixed = FOI_to_HC_list_fixed,
#                                         FOI_to_R0_1_list = FOI_to_R0_1_list,
#                                         FOI_to_R0_2_list = FOI_to_R0_2_list,
#                                         FOI_to_R0_3_list = FOI_to_R0_3_list,
#                                         parms = parameters,
#                                         out_path = out_path,
#                                         var_to_fit = fit_var))


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
                            var_to_fit = fit_var)
  
} else {
  
  burden <- loop(fctr_combs, 
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
  
}

if (!CLUSTER){
  
  context::parallel_cluster_stop()
  
}
