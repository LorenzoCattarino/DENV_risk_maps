# Calculates
# for each model fit, 
# R0 scenario and 
# 20 km square (assuming optimal choice of screening age):
#
# 1) number of infections
# 2) number of cases
# 3) number of hospitalized cases 

options(didehpc.cluster = "fi--didemrchnb")

CLUSTER <- FALSE

my_resources <- c(
  file.path("R", "utility_functions.R"),
  file.path("R", "create_parameter_list.R"),
  file.path("R", "prepare_datasets", "average_up.r"),
  file.path("R", "burden_and_interventions", "wrappers_to_vaccine_impact_calculation.R"))

my_pkgs <- c("dplyr", "matrixStats") # for rowMaxs

context::context_log_start()
ctx <- context::context_save(path = "context",
                             packages = my_pkgs,
                             sources = my_resources)


# define parameters -----------------------------------------------------------  


extra_prms <- list(id = 2,
                   dependent_variable = "FOI",
                   R0_scenario = 2,
                   wolbachia_scenario_id = 4,
                   no_R0_assumptions = 4,
                   screening_ages = 0,
                   parallel_2 = TRUE,
                   burden_measure = c("infections", "cases", "hosp"),
                   vacc_estimates = c("mean", "L95", "U95"),
                   phi_set_id_tag = "phi_set_id",
                   base_info = c("population", "ID_0", "ID_1"))


# are you using the cluster? --------------------------------------------------


if (CLUSTER) {
  
  config <- didehpc::didehpc_config(template = "20Core")
  obj <- didehpc::queue_didehpc(ctx, config = config)
  
} else {
  
  context::context_load(ctx)
  context::parallel_cluster_start(8, ctx)
  
}


# define variables ------------------------------------------------------------


parameters <- create_parameter_list(extra_params = extra_prms)

var_to_fit <- parameters$dependent_variable

R0_scenario <- parameters$R0_scenario

vacc_estimates <- parameters$vacc_estimates

burden_measures <- parameters$burden_measure

screening_ages <- parameters$screening_ages

w_scenario_id <- parameters$wolbachia_scenario_id

model_type <- paste0("model_", parameters$id)

predictions_file_name <- paste0("transformed_", R0_scenario, "_wolbachia_", w_scenario_id, ".rds")

out_path <- file.path("output", 
                      "predictions_world", 
                      "bootstrap_models",
                      model_type,
                      "adm_1")

phi_set_id_tag <- parameters$phi_set_id_tag


# load data -------------------------------------------------------------------  


sqr_preds <- readRDS(file.path("output", 
                             "predictions_world", 
                             "bootstrap_models",
                             model_type, 
                             "adm_1",
                             predictions_file_name))



# create table of scenarios --------------------------------------------------- 


phi_set_id <- seq_len(parameters$no_R0_assumptions)

fct_c <- setNames(expand.grid(phi_set_id, 
                              burden_measures, 
                              screening_ages,
                              vacc_estimates,
                              stringsAsFactors = FALSE),
                  nm = c(phi_set_id_tag, "burden_measure", "screening_age", "estimate"))

fct_c <- cbind(id = seq_len(nrow(fct_c)), fct_c)

if(var_to_fit == "FOI"){
  
  assumption <- 4 
  
} else {
  
  assumption <- as.numeric(unlist(strsplit(var_to_fit, "_"))[2])
  
}

fct_c_2 <- subset(fct_c, phi_set_id == assumption)  

write_out_csv(fct_c_2, out_path, "scenario_table_vaccine.csv", row.names = FALSE)

fctr_combs <- df_to_list(fct_c_2, use_names = TRUE)


# pre processing -------------------------------------------------------------- 


sqr_preds <- as.matrix(sqr_preds)


# submit ----------------------------------------------------------------------


if (CLUSTER) {
  
  vaccine_impact <- queuer::qlapply(fctr_combs,
                                    wrapper_to_multi_factor_vaccine_impact,
                                    obj,
                                    preds = sqr_preds, 
                                    parms = parameters, 
                                    out_path = out_path)
  
} else {
  
  vaccine_impact <- loop(fctr_combs, 
                         wrapper_to_multi_factor_vaccine_impact,
                         preds = sqr_preds, 
                         parms = parameters, 
                         out_path = out_path,
                         parallel = FALSE)
  
}

if (!CLUSTER){
  
  context::parallel_cluster_stop()
  
}
