# Calculates look up tables for burden calculation

# 1) FOI -> number of infections
# 2) FOI -> number of cases
# 3) FOI -> number of hospitalized cases 

options(didehpc.cluster = "fi--didemrchnb")

CLUSTER <- TRUE

my_resources <- c(
  file.path("R", "burden_and_interventions", "functions_for_calculating_burden.R"),
  file.path("R", "burden_and_interventions", "create_burden_lookup.R"),
  file.path("R", "prepare_datasets", "functions_for_calculating_R0.R"),
  file.path("R", "utility_functions.R"),
  file.path("R", "create_parameter_list.R"))

my_pkgs <- c("data.table", "dplyr")

context::context_log_start()
ctx <- context::context_save(path = "context",
                             packages = my_pkgs,
                             sources = my_resources)


# define extra parameters -----------------------------------------------------


extra_prms <- list(id = 22,
                   prop_sym_parms_nm = c("gamma_1", "rho", "gamma_3"))


# are you using the cluster? --------------------------------------------------


if (CLUSTER) {
  
  config <- didehpc::didehpc_config(template = "24Core")
  obj <- didehpc::queue_didehpc(ctx, config = config)
  
} else {
  
  context::context_load(ctx)
  context::parallel_cluster_start(7, ctx)
  
}


# define variables ------------------------------------------------------------


parameters <- create_parameter_list(extra_params = extra_prms)

model_type <- paste0("model_", parameters$id)

out_path <- file.path("output", 
                      "predictions_world", 
                      "bootstrap_models", 
                      model_type)

no_samples <- parameters$no_samples
  
FOI_values <- parameters$FOI_grid

prop_sym_parms_nm <- parameters$prop_sym_parms_nm


# load data ------------------------------------------------------------------- 


age_struct <- read.csv(file.path("output", 
                                 "datasets",
                                 "country_age_structure.csv"), 
                       header = TRUE) 

param_post <- read.table(file.path("data", "sym_prop_param_post.txt"))


# pre processing --------------------------------------------------------------


age_band_tgs <- grep("band", names(age_struct), value = TRUE)
age_band_bnds <- get_age_band_bounds(age_band_tgs)
age_band_L_bounds <- age_band_bnds[, 1]
age_band_U_bounds <- age_band_bnds[, 2] + 1

param_post <- param_post[seq_len(no_samples), -c(1, 5)]
  
param_post <- as.matrix(param_post)

colnames(param_post) <- prop_sym_parms_nm

target_fun <- list("calculate_cases", "calculate_hosp_cases")

target_nm <- list("C", "HC")


# create FOI -> Inf lookup table ---------------------------------------------- 


if(!file.exists(file.path(out_path, "FOI_to_I_lookup_tables.rds"))){
  
  Infection_values <- loop(seq_len(nrow(age_struct)), 
                           wrapper_to_lookup,
                           age_struct = age_struct, 
                           tags = age_band_tgs, 
                           FOI_values = FOI_values, 
                           my_fun = "calculate_infections",
                           age_band_lower_bounds = age_band_L_bounds,
                           age_band_upper_bounds = age_band_U_bounds,
                           parallel = TRUE)
  
  FOI_to_Inf_list <- lapply(Infection_values, cbind_FOI_to_lookup, FOI_values)
  
  saveRDS(FOI_to_Inf_list, file.path(out_path, "FOI_to_I_lookup_tables.rds"))
  
} else {
  
  FOI_to_Inf_list <- readRDS(file.path(out_path, "FOI_to_I_lookup_tables.rds"))
  
}


# submit one job --------------------------------------------------------------


# lookup <- obj$enqueue(
#   create_lookup_tables(seq_along(target_fun)[[1]],
#                        target_fun = target_fun,
#                        target_nm = target_nm,
#                        out_path = out_path,
#                        age_struct = age_struct,
#                        age_band_tgs = age_band_tgs,
#                        FOI_values = FOI_values,
#                        age_band_L_bounds = age_band_L_bounds,
#                        age_band_U_bounds = age_band_U_bounds,
#                        param_post = param_post,
#                        parms = parameters,
#                        parallel = TRUE))


# submit a bundle -------------------------------------------------------------


if (CLUSTER) {

  lookup <- queuer::qlapply(seq_along(target_fun),
                            create_lookup_tables,
                            obj,
                            target_fun = target_fun,
                            target_nm = target_nm,
                            out_path = out_path,
                            age_struct = age_struct,
                            age_band_tgs = age_band_tgs,
                            FOI_values = FOI_values,
                            age_band_L_bounds = age_band_L_bounds,
                            age_band_U_bounds = age_band_U_bounds,
                            param_post = param_post,
                            parms = parameters,
                            parallel = TRUE)

} else {

  lookup <- loop(seq_along(target_fun),
                 create_lookup_tables,
                 target_fun = target_fun,
                 target_nm = target_nm,
                 out_path = out_path,
                 age_struct = age_struct,
                 age_band_tgs = age_band_tgs,
                 FOI_values = FOI_values,
                 age_band_L_bounds = age_band_L_bounds,
                 age_band_U_bounds = age_band_U_bounds,
                 param_post = param_post,
                 parms = parameters,
                 parallel = TRUE)

}

if (!CLUSTER){

  context::parallel_cluster_stop()

}
