# 1. Makes foi predictions of all squares, for each tile and model fit

# 2. Conditionally calculates, for each tile, model fit and R0-Wolbachia effect assumption combinations
# R0 values and burden, for different foi value (squares).

# 3. Takes the mean and confidence intervals, across models,  
# of different measured variables (foi, R0, burden), for each tile. 

options(didehpc.cluster = "fi--didemrchnb")

CLUSTER <- TRUE

my_resources <- c(
  file.path("R", "utility_functions.r"),
  file.path("R", "random_forest", "wrapper_to_load_tile_dataset.r"),
  file.path("R", "random_forest", "wrapper_to_make_predictions.r"),
  file.path("R", "random_forest", "make_h2o_RF_predictions.r"),
  file.path("R", "burden_and_interventions", "wrapper_to_multi_factor_burden.r"),
  file.path("R", "burden_and_interventions", "wrapper_to_get_multi_foi_R0.r"),
  file.path("R", "burden_and_interventions", "wrapper_to_get_R0.r"),             
  file.path("R", "burden_and_interventions", "get_age_band_bounds.r"),
  file.path("R", "burden_and_interventions", "calculate_infection_probability_and_number.r"),
  file.path("R", "burden_and_interventions", "calculate_average_infect_probab.r"),
  file.path("R", "burden_and_interventions", "calculate_total_incidence.r"),
  file.path("R", "burden_and_interventions", "calculate_incidence_of_infections.r"),
  file.path("R", "burden_and_interventions", "calculate_R0.r"),
  file.path("R", "prepare_datasets", "calculate_mean_across_fits.r"),
  file.path("R", "prepare_datasets", "wrapper_to_mean_across_fits.r"))

my_pkgs <- c("h2o", "data.table", "approximate", "dplyr")

gth <- provisionr::package_sources(github = "richfitz/approximate")
context::context_log_start()
ctx <- context::context_save(path = "context",
                             packages = my_pkgs,
                             sources = my_resources,
                             package_sources = gth)


# ---------------------------------------- define parameters


model_tp <- "boot_model_20km_cw"

no_fits <- 50

NA_tile_fl_name <- "NA_pixel_tiles_20km.txt"

in_pth <- file.path("output", "env_variables", "all_sets_0_1667_deg", "gadm")

# there is always `foi` and it always comes first
var_names <- c("foi", "R0", "I_num", "C_num")#, "I_inc", "C_inc") 

bs_inf <- c("cell", "lat.grid", "long.grid", "population")

# percentage reduction in R0 caused by wolbachia
sf_vals <- 1

# id of the combinations of infectiousness weights (i.e., how many infections you can have before immunity)
phi_set_id <- c(1, 2, 3, 4)

phi_set_id_tag <- "phi_set_id"

# relative simptomaticity of prim second and tert infections 
w_1 <- 0.45
w_2 <- 0.85
w_3 <- 0.15


# ---------------------------------------- define variables


RF_obj_path <- file.path(
  "output",
  "EM_algorithm",
  model_tp,
  "optimized_model_objects")

out_pth <- file.path(
  "output", 
  "predictions_world", 
  model_tp,
  "tile_sets_0_1667_deg")


# ---------------------------------------- are you using the cluster?


if (CLUSTER) {
  
  config <- didehpc::didehpc_config(template = "12and16Core")
  obj <- didehpc::queue_didehpc(ctx, config = config)
  
} else {
  
  context::context_load(ctx)
  context::parallel_cluster_start(8, ctx)
  
}


# ---------------------------------------- load data


# predicting variable rank
predictor_rank <- read.csv(
  file.path("output", 
            "variable_selection", 
            "metropolis_hastings", 
            "exp_1", 
            "variable_rank_final_fits_exp_1.csv"),
  stringsAsFactors = FALSE)

# tiles
tile_summary <- read.csv(
  file.path("data", 
            "env_variables", 
            "plus60minus60_tiles.csv"), 
  header = TRUE, 
  sep = ",", 
  stringsAsFactors = FALSE)

# NA pixel tiles 
NA_pixel_tiles <- read.table(
  file.path("output", 
            "datasets", 
            NA_tile_fl_name), 
  sep = ",", 
  header = TRUE)

age_struct <- read.csv(
  file.path("output", 
            "datasets",
            "country_age_structure.csv"), 
  header = TRUE) 


# ---------------------------------------- create df with different combinations of infectiousness weigths (phi)  


v1 <- c(1, 1, 0, 0) # Up to 2 infections
v2 <- c(1, 1, 1, 0) # Up to 3 infections
v3 <- c(1, 1, 1, 1) # Up to 4 infections 

phi_2 <- 1
phi_1 <- (w_1 * 2 + (1 - w_1)) / (w_2 * 2 + (1 - w_2)) 
phi_3 <- phi_4 <- (w_3 * 2 + (1 - w_3)) / (w_2 * 2 + (1 - w_2))

v4 <- c(phi_1, phi_2, phi_3, phi_4) # up to 4, with symptomatic infections twice as infectious as asymptomatic ones

phi_combs <- setNames(data.frame(seq_len(4), rbind(v1, v2, v3, v4)),
                      nm = c(phi_set_id_tag, "phi1", "phi2", "phi3", "phi4"))


# ---------------------------------------- extract info from age structure 


age_band_tgs <- grep("band", names(age_struct), value = TRUE)

age_band_bnds <- get_age_band_bounds(age_band_tgs)

age_band_L_bounds <- age_band_bnds[, 1]

age_band_U_bounds <- age_band_bnds[, 2] + 1


# ---------------------------------------- get the vector of predictors


best_predictors <- predictor_rank$variable[1:9]


# ---------------------------------------- get tile ids


tile_ids <- tile_summary$tile.id

NA_pixel_tile_ids <- NA_pixel_tiles$tile_id

tile_ids_2 <- tile_ids[!tile_ids %in% NA_pixel_tile_ids]  


# ---------------------------------------- create combinations of phi sets and wolbachia effect


fct_c <- setNames(expand.grid(phi_set_id, sf_vals),
                  nm = c(phi_set_id_tag, "scaling_factor"))

fct_c <- cbind(id = seq_len(nrow(fct_c)), fct_c)

fct_c_2 <- left_join(fct_c, phi_combs, by = phi_set_id_tag)

fctr_combs <- df_to_list(fct_c_2, use_names = TRUE)


# ---------------------------------------- submit one job 


# t <- obj$enqueue(
#   wrapper_to_load_tile_dataset(
#     seq_along(tile_ids_2)[261],
#     ids_vec = tile_ids_2,
#     in_path = in_pth,
#     no_fits = no_fits,
#     model_in_path = RF_obj_path,
#     predictors = best_predictors,
#     burden = TRUE,
#     age_struct = age_struct,
#     var_names = var_names,
#     fctr_combs = fctr_combs,
#     age_band_tgs = age_band_tgs,
#     age_band_lower_bounds = age_band_L_bounds,
#     age_band_upper_bounds = age_band_U_bounds,
#     w_1 = w_1,
#     w_2 = w_2,
#     w_3 = w_3,
#     base_info = bs_inf,
#     out_path = out_pth))


# ---------------------------------------- submit all jobs


if (CLUSTER) {

  pred_tiles <- queuer::qlapply(
    seq_along(tile_ids_2),
    wrapper_to_load_tile_dataset,
    obj,
    ids_vec = tile_ids_2,
    in_path = in_pth,
    no_fits = no_fits,
    model_in_path = RF_obj_path,
    predictors = best_predictors,
    burden = TRUE,
    age_struct = age_struct,
    var_names = var_names,
    fctr_combs = fctr_combs,
    age_band_tgs = age_band_tgs,
    age_band_lower_bounds = age_band_L_bounds,
    age_band_upper_bounds = age_band_U_bounds,
    w_1 = w_1,
    w_2 = w_2,
    w_3 = w_3,
    base_info = bs_inf,
    out_path = out_pth)

} else {

  pred_tiles <- lapply(
    seq_along(tile_ids_2)[261],
    wrapper_to_load_tile_dataset,
    ids_vec = tile_ids_2,
    in_path = in_pth,
    no_fits = no_fits,
    model_in_path = RF_obj_path,
    predictors = best_predictors,
    burden = TRUE,
    age_struct = age_struct,
    var_names = var_names,
    fctr_combs = fctr_combs,
    age_band_tgs = age_band_tgs,
    age_band_lower_bounds = age_band_L_bounds,
    age_band_upper_bounds = age_band_U_bounds,
    w_1 = w_1,
    w_2 = w_2,
    w_3 = w_3,
    base_info = bs_inf,
    out_path = out_pth)

}

if (!CLUSTER) {
  context::parallel_cluster_stop()
}
