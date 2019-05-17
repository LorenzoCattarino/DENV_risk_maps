# Calculates look up tables:

# 1) R0_1 -> FOI
# 2) R0_2 -> FOI
# 3) R0_3 -> FOI
# 4) FOI -> Infections
# 5) FOI -> Cases
# 6) FOI -> Hospitalized cases 

# using a fixed set of values of 
# proportions of infections which are symptomatic.

# Calculates look up tables:

# 2) FOI -> Cases
# 3) FOI -> Hospitalized cases 

# using also the posterior distribution of
# proportions of infections which are symptomatic.


# -----------------------------------------------------------------------------


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


extra_prms <- list(target_nm = list("I", "C", "HC", "R0_1", "R0_2", "R0_3"),
                   parallel_2 = TRUE)


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

out_path <- file.path("output", 
                      "predictions_world")

no_samples <- parameters$no_samples

mean_prop_sympt <- parameters$prop_sympt

target_nm <- parameters$target_nm


# load data ------------------------------------------------------------------- 


age_struct <- read.csv(file.path("output", 
                                 "datasets",
                                 "country_age_structure.csv"), 
                       header = TRUE) 

param_post <- read.table(file.path("data", "sym_prop_param_post.txt"),
                         header = TRUE)


# pre processing --------------------------------------------------------------


age_band_tgs <- grep("band", names(age_struct), value = TRUE)
age_band_bnds <- get_age_band_bounds(age_band_tgs)
age_band_L_bounds <- age_band_bnds[, 1]
age_band_U_bounds <- age_band_bnds[, 2] + 1

param_post <- param_post[seq_len(no_samples), -c(1, 5)]
  
param_post <- as.matrix(param_post)

param_post_inf_weights <- t(apply(param_post, 
                                  1, 
                                  calculate_infectiousness_wgts_for_sym_asym_assumption))

target_fun <- list(calculate_infections = "calculate_infections",
                   calculate_cases = "calculate_cases", 
                   calculate_hosp_cases = "calculate_hosp_cases", 
                   calculate_R0 = "calculate_R0")

additional_dts <- list(prop_sym = param_post, inf_weights = param_post_inf_weights)

vec_phis_R0_3 <- calculate_infectiousness_wgts_for_sym_asym_assumption(mean_prop_sympt)

parameters <- c(parameters, list(vec_phis_R0_3 = vec_phis_R0_3))


# submit one job --------------------------------------------------------------


# lookup <- obj$enqueue(
#   create_lookup_tables(target_nm[[1]],
#                        target_fun = target_fun,
#                        additional_dts = additional_dts,
#                        out_path = out_path,
#                        age_struct = age_struct,
#                        age_band_tags = age_band_tgs,
#                        age_band_L_bounds = age_band_L_bounds,
#                        age_band_U_bounds = age_band_U_bounds,
#                        parms = parameters))


# submit a bundle -------------------------------------------------------------


if (CLUSTER) {

  lookup <- queuer::qlapply(target_nm,
                            create_lookup_tables,
                            obj,
                            target_fun = target_fun,
                            additional_dts = additional_dts,
                            out_path = out_path,
                            age_struct = age_struct,
                            age_band_tags = age_band_tgs,
                            age_band_L_bounds = age_band_L_bounds,
                            age_band_U_bounds = age_band_U_bounds,
                            parms = parameters)

} else {

  lookup <- loop(target_nm,
                 create_lookup_tables,
                 target_fun = target_fun,
                 additional_dts = additional_dts,
                 out_path = out_path,
                 age_struct = age_struct,
                 age_band_tags = age_band_tgs,
                 age_band_L_bounds = age_band_L_bounds,
                 age_band_U_bounds = age_band_U_bounds,
                 parms = parameters,
                 parallel = FALSE)

}

if (!CLUSTER){

  context::parallel_cluster_stop()

}
