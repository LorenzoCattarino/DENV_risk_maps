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

CLUSTER <- TRUE

my_resources <- c(
  file.path("R", "utility_functions.R"),
  file.path("R", "create_parameter_list.R"),
  file.path("R", "prepare_datasets", "functions_for_calculating_R0.R"),
  file.path("R", "burden_and_interventions", "functions_for_calculating_burden.R"),
  file.path("R", "burden_and_interventions", "wrapper_to_multi_factor_R0_and_burden.R"),
  file.path("R", "burden_and_interventions", "wrapper_to_replicate_R0_and_burden.R"))

my_pkgs <- "dplyr"

context::context_log_start()
ctx <- context::context_save(path = "context",
                             packages = my_pkgs,
                             sources = my_resources)


# define extra parameters -----------------------------------------------------


extra_prms <- list(id = 4,
                   dependent_variable = "FOI",
                   no_R0_assumptions = 4,
                   fit_type = "boot",
                   parallel_2 = TRUE,
                   phi_set_id_tag = "phi_set_id",
                   base_info = c("cell", 
                                 "latitude", 
                                 "longitude", 
                                 "population", 
                                 "ID_0", 
                                 "ID_1", 
                                 "ID_2"))


# are you using the cluster? --------------------------------------------------


if (CLUSTER) {
  
  config <- didehpc::didehpc_config(template = "GeneralNodes", wholenode = TRUE)
  obj <- didehpc::queue_didehpc(ctx, config = config)
  
} else {
  
  context::context_load(ctx)
  context::parallel_cluster_start(8, ctx)
  
}


# define variables ------------------------------------------------------------


parameters <- create_parameter_list(extra_params = extra_prms)

var_to_fit <- parameters$dependent_variable

model_type <- paste0("model_", parameters$id)

lookup_path <- file.path("output", 
                         "predictions_world")

out_path <- file.path("output", 
                      "predictions_world", 
                      "bootstrap_models", 
                      model_type)

sf_vals <- parameters$sf_vals

phi_set_id_tag <- parameters$phi_set_id_tag

no_R0_assumptions <- parameters$no_R0_assumptions


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
                               "bootstrap_models",
                               model_type,
                               "response.rds"))

age_struct <- read.csv(file.path("output", 
                                 "datasets",
                                 "country_age_structure.csv"), 
                       header = TRUE) 

endemic_c <- read.csv(file.path("output", 
                                "datasets", 
                                "dengue_endemic_countries.csv"),
                      stringsAsFactors = FALSE)


# pre processing --------------------------------------------------------------


age_band_tgs <- grep("band", names(age_struct), value = TRUE)
age_band_bnds <- get_age_band_bounds(age_band_tgs)
age_band_L_bounds <- age_band_bnds[, 1]
age_band_U_bounds <- age_band_bnds[, 2] + 1

age_struct$age_id <- seq_len(nrow(age_struct))

# keep only the predictions for which there is age data available
sqr_preds_2 <- inner_join(age_struct[, c("age_id", "ID_0")],
                          sqr_preds, 
                          by = "ID_0")

# keep only endemic countries
sqr_preds_3 <- inner_join(sqr_preds_2, endemic_c[, "ID_0", drop = FALSE], by = "ID_0")

sqr_preds_3 <- as.matrix(sqr_preds_3)

# sqr_preds_3 <- sqr_preds_3[52697:52707,]


# create table of scenarios --------------------------------------------------- 


phi_set_id <- seq_len(no_R0_assumptions)

fct_c <- setNames(expand.grid(phi_set_id, sf_vals),
                  nm = c(phi_set_id_tag, "scaling_factor"))

fct_c <- cbind(id = seq_len(nrow(fct_c)), fct_c)

if(var_to_fit == "FOI"){
  
  assumption <- 4 

} else {
  
  assumption <- as.numeric(unlist(strsplit(var_to_fit, "_"))[2])

}

fct_c <- subset(fct_c, phi_set_id == assumption)

write_out_csv(fct_c, out_path, "scenario_table_wolbachia.csv", row.names = FALSE)

fctr_combs <- df_to_list(fct_c, use_names = TRUE)


# submit one job --------------------------------------------------------------


# t <- obj$enqueue(
#   wrapper_to_multi_factor_R0_and_burden(fctr_combs[[1]],
#                                         foi_data = sqr_preds_3,
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
#                                         out_path = out_path))


# submit a bundle -------------------------------------------------------------


if (CLUSTER) {

  burden <- queuer::qlapply(fctr_combs,
                            wrapper_to_multi_factor_R0_and_burden,
                            obj,
                            foi_data = sqr_preds_3,
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
                            out_path = out_path)

} else {

  burden <- loop(fctr_combs,
                 wrapper_to_multi_factor_R0_and_burden,
                 foi_data = sqr_preds_3,
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
                 parallel = FALSE)

}

if (!CLUSTER){

  context::parallel_cluster_stop()

}
