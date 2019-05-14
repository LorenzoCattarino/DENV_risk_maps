# Run the entire predicting routine for each tile 

options(didehpc.cluster = "fi--didemrchnb")

CLUSTER <- TRUE

my_resources <- c(
  file.path("R", "utility_functions.r"),
  file.path("R", "random_forest", "wrapper_to_predict_pixels.R"),
  file.path("R", "random_forest", "functions_for_fitting_h2o_RF_and_making_predictions.r"),
  file.path("R", "burden_and_interventions", "wrapper_to_multi_factor_R0_and_burden.r"),
  file.path("R", "burden_and_interventions", "wrapper_to_replicate_R0_and_burden.r"),  
  file.path("R", "burden_and_interventions", "wrapper_to_R0_and_burden.r"),
  file.path("R", "burden_and_interventions", "functions_for_calculating_burden.r"),
  file.path("R", "prepare_datasets", "functions_for_calculating_R0.r"),
  file.path("R", "burden_and_interventions", "calculate_mean_across_fits.r"))

my_pkgs <- c("h2o", "data.table", "dplyr")

context::context_log_start()
ctx <- context::context_save(path = "context",
                             sources = my_resources,
                             packages = my_pkgs)


# define parameters ----------------------------------------------------------- 


var_to_fit <- "FOI"

fit_type <- "boot"

model_tp <- paste0(var_to_fit, "_", fit_type, "_model")

no_fits <- 200

RF_mod_name <- "RF_obj_sample"

var_names <- c("R0_r", "I_num", "C_num", "I_inc", "C_inc")

var_to_average <- c("FOI", "R0_r")
  
base_info <- c("cell", "lat.grid", "long.grid", 
               "population", "ADM_0", "ADM_1", "ADM_2")

phi_set_id <- c(1, 3, 4)
sf_vals <- c(1, 0.7, 0.3)
phi_set_id_tag <- "phi_set_id"
gamma_1 <- 0.45
rho <- 0.85
gamma_3 <- 0.15

v1 <- c(1, 1, 0, 0) # Up to 2 infections
v2 <- c(1, 1, 1, 0) # Up to 3 infections
v3 <- c(1, 1, 1, 1) # Up to 4 infections 

FOI_values <- seq(0, 0.2, by = 0.0002)

prob_fun <- list("calculate_primary_infection_prob",
                 "calculate_secondary_infection_prob",
                 "calculate_tertiary_infection_prob",
                 "calculate_quaternary_infection_prob")

NA_pixel_tile_ids <- c(122, 217, 254, 
                       284, 288, 289,
                       290, 291, 321, 
                       326, 406, 432, 
                       433, 485, 529)

scenario_ids <- 1:3

dts_tag <- "all_squares"

tile_path <- file.path("output", "env_variables", "tile_set_5_km", "gadm")

prediction_path <- file.path("output", "predictions_world", model_tp)

RF_obj_path <- file.path("output", 
                         "EM_algorithm", 
                         model_tp, 
                         "optimized_model_objects")

out_pth <- file.path(prediction_path, "tile_set_5_km")


# are you using the cluster ? ------------------------------------------------- 


if (CLUSTER) {
  
  config <- didehpc::didehpc_config(template = "20Core")
  obj <- didehpc::queue_didehpc(ctx, config = config)

} else {
  
  context::context_load(ctx)
  context::parallel_cluster_start(8, ctx)

}


# load data ------------------------------------------------------------------- 


predictor_rank <- read.csv(
  file.path("output", 
            "variable_selection", 
            "metropolis_hastings", 
            "exp_1", 
            "variable_rank_final_fits_exp_1.csv"),
  stringsAsFactors = FALSE)

tile_summary <- read.csv(
  file.path("data", 
            "env_variables", 
            "plus60minus60_tiles.csv"), 
  header = TRUE, 
  sep = ",", 
  stringsAsFactors = FALSE)

age_struct <- read.csv(
  file.path("output", 
            "datasets",
            "country_age_structure.csv"), 
  header = TRUE) 

FOI_to_Inf_list <- readRDS(file.path(prediction_path, 
                                     "FOI_to_I_lookup_tables.rds"))

FOI_to_C_list <- readRDS(file.path(prediction_path, 
                                   "FOI_to_C_lookup_tables.rds"))


# get vector of predictors ----------------------------------------------------  


predictors <- predictor_rank$variable[1:9]


# create table of scenarios ---------------------------------------------------  


v4 <- calculate_infectiousness_wgts_for_sym_asym_assumption(gamma_1, 
                                                            rho, 
                                                            gamma_3)
phi_combs <- setNames(data.frame(seq_len(4), rbind(v1, v2, v3, v4)),
                      nm = c(phi_set_id_tag, "phi1", "phi2", "phi3", "phi4"))

fct_c <- setNames(expand.grid(phi_set_id, sf_vals),
                  nm = c(phi_set_id_tag, "scaling_factor"))

fct_c <- cbind(id = seq_len(nrow(fct_c)), fct_c)

fct_c_2 <- left_join(fct_c, phi_combs, by = phi_set_id_tag)

fctr_combs <- df_to_list(fct_c_2, use_names = TRUE)


# pre-process age structure dataset -------------------------------------------


age_struct$age_id <- seq_len(nrow(age_struct))

names(age_struct)[names(age_struct) == "ID_0"] <- "ADM_0"


# get age group limits --------------------------------------------------------


age_band_tgs <- grep("band", names(age_struct), value = TRUE)
age_band_bnds <- get_age_band_bounds(age_band_tgs)
age_band_L_bounds <- age_band_bnds[, 1]
age_band_U_bounds <- age_band_bnds[, 2] + 1


# get tile ids ---------------------------------------------------------------- 


tile_ids <- tile_summary$tile.id

tile_ids_2 <- tile_ids[!tile_ids %in% NA_pixel_tile_ids]  


# submit one job -------------------------------------------------------------- 


t <- obj$enqueue(
  wrapper_to_predict_pixels(
    seq_along(tile_ids_2)[136],
    ids_vec = tile_ids_2,
    in_path = tile_path,
    no_fits = no_fits,
    RF_mod_name = RF_mod_name,
    model_in_path = RF_obj_path,
    predictors = predictors,
    fctr_combs = fctr_combs,
    age_struct = age_struct,
    age_band_tgs = age_band_tgs,
    age_band_lower_bounds = age_band_L_bounds,
    age_band_upper_bounds = age_band_U_bounds,
    var_names = var_names,
    FOI_values = FOI_values,
    FOI_to_Inf_list = FOI_to_Inf_list,
    FOI_to_C_list = FOI_to_C_list,
    prob_fun = prob_fun,
    var_to_fit = var_to_fit,
    fit_type = fit_type,
    base_info = base_info,
    lookup_path = prediction_path,
    out_path = out_pth,
    var_to_average = var_to_average,
    scenario_ids = scenario_ids,
    dts_tag = dts_tag))


# submit all jobs -------------------------------------------------------------


# if (CLUSTER) {
# 
#   all_tiles <- queuer::qlapply(
#     seq_along(tile_ids_2),
#     wrapper_to_predict_pixels,
#     obj,
#     ids_vec = tile_ids_2,
#     in_path = tile_path,
#     no_fits = no_fits,
#     RF_mod_name = RF_mod_name,
#     model_in_path = RF_obj_path,
#     predictors = predictors,
#     fctr_combs = fctr_combs,
#     age_struct = age_struct,
#     age_band_tgs = age_band_tgs,
#     age_band_lower_bounds = age_band_L_bounds,
#     age_band_upper_bounds = age_band_U_bounds,
#     var_names = var_names,
#     FOI_values = FOI_values, 
#     FOI_to_Inf_list = FOI_to_Inf_list,
#     FOI_to_C_list = FOI_to_C_list,
#     prob_fun = prob_fun,
#     var_to_fit = var_to_fit,
#     fit_type = fit_type,
#     base_info = base_info,
#     lookup_path = prediction_path,
#     out_path = out_pth,
#     var_to_average = var_to_average,
#     scenario_ids = scenario_ids,
#     dts_tag = dts_tag)
# 
# } else {
# 
#   all_tiles <- lapply(
#     seq_along(tile_ids_2)[1],
#     wrapper_to_predict_pixels,
#     ids_vec = tile_ids_2,
#     in_path = tile_path,
#     no_fits = no_fits,
#     RF_mod_name = RF_mod_name,
#     model_in_path = RF_obj_path,
#     predictors = predictors,
#     fctr_combs = fctr_combs,
#     age_struct = age_struct,
#     age_band_tgs = age_band_tgs,
#     age_band_lower_bounds = age_band_L_bounds,
#     age_band_upper_bounds = age_band_U_bounds,
#     var_names = var_names,
#     FOI_values = FOI_values, 
#     FOI_to_Inf_list = FOI_to_Inf_list,
#     FOI_to_C_list = FOI_to_C_list,
#     prob_fun = prob_fun,
#     var_to_fit = var_to_fit,
#     fit_type = fit_type,
#     base_info = base_info,
#     lookup_path = prediction_path,
#     out_path = out_pth,
#     var_to_average = var_to_average,
#     scenario_ids = scenario_ids,
#     dts_tag = dts_tag)
# 
# }
# 
# if (!CLUSTER) {
#   context:::parallel_cluster_stop()
# }
