# Calculates, for each tile, model fit and R0-Wolbachia effect assumption combinations
# R0 values and burden, for different foi value (squares).

options(didehpc.cluster = "fi--didemrchnb")

CLUSTER <- TRUE

my_resources <- c(
  file.path("R", "utility_functions.r"),
  file.path("R", "burden_and_interventions", "wrapper_to_burden_and_wolbachia_one_tile.r"),
  file.path("R", "burden_and_interventions", "wrapper_to_multi_factor_burden.r"),
  file.path("R", "burden_and_interventions", "wrapper_to_get_multi_foi_R0.r"),
  file.path("R", "burden_and_interventions", "wrapper_to_get_R0.r"),             
  file.path("R", "burden_and_interventions", "get_age_band_bounds.r"),
  file.path("R", "burden_and_interventions", "calculate_infection_probability_and_number.r"),
  file.path("R", "burden_and_interventions", "calculate_average_infect_probab.r"),
  file.path("R", "burden_and_interventions", "calculate_total_incidence.r"),
  file.path("R", "burden_and_interventions", "calculate_incidence_of_infections.r"),
  file.path("R", "burden_and_interventions", "calculate_R0.r"))

my_pkgs <- c("data.table", "approximate", "dplyr")

gth <- provisionr::package_sources(github = "richfitz/approximate")
context::context_log_start()
ctx <- context::context_save(path = "context",
                             sources = my_resources,
                             packages = my_pkgs,
                             package_sources = gth)


# ---------------------------------------- define parameters


model_tp <- "boot_model_20km_cw" 

out_fl_nm <- "pred_0_1667_deg_long.rds"

NA_tile_fl_name <- "NA_pixel_tiles_20km.txt"

# percentage reduction in R0 caused by wolbachia
sf_vals <- 1

# id of the combinations of infectiousness weights (i.e., how many infections you can have before immunity)
phi_set_id <- c(1, 2, 3, 4)

phi_set_id_tag <- "phi_set_id"

# relative simptomaticity of prim second and tert infections 
w_1 <- 0.45
w_2 <- 0.85
w_3 <- 0.15


# ---------------------------------------- are you using the cluster?


if (CLUSTER) {
  
  config <- didehpc::didehpc_config(template = "12and16Core")
  obj <- didehpc::queue_didehpc(ctx, config = config)
  
} else {
  
  context::context_load(ctx)
  #context::parallel_cluster_start(8, ctx)
  
}


# ---------------------------------------- load data


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


# ---------------------------------------- get tile ids


# get tile ids
tile_ids <- tile_summary$tile.id

NA_pixel_tile_ids <- NA_pixel_tiles$tile_id

tile_ids_2 <- tile_ids[!tile_ids %in% NA_pixel_tile_ids]  


# ---------------------------------------- create combinations of phi sets and wolbachia effect


fct_c <- setNames(expand.grid(phi_set_id, sf_vals),
                       nm = c(phi_set_id_tag, "scaling_factor"))

fct_c <- cbind(id = seq_len(nrow(fct_c)), fct_c)

fct_c_2 <- left_join(fct_c, phi_combs, by = phi_set_id_tag)

fctr_combs <- df_to_list(fct_c_2, use_names = TRUE)


# ---------------------------------------- define variables 


orig_in_pth  <- file.path(
  "output", 
  "env_variables", 
  "all_sets_0_1667_deg", 
  "gadm")

in_pth <- file.path(
  "output", 
  "predictions_world", 
  model_tp,
  "tile_sets_0_1667_deg")

out_pth_all <- file.path(
  "output", 
  "predictions_world", 
  model_tp,
  "tile_sets_0_1667_deg",
  paste0("tile_", tile_ids_2))


# ---------------------------------------- submit one job 


# t <- obj$enqueue(
#   burden_and_wolbachia_one_tile(
#     seq_along(tile_ids_2)[1],
#     ids_vec = tile_ids_2,
#     in_path = in_pth,
#     orig_in_path = orig_in_pth,
#     xx = fctr_combs,
#     age_band_tgs = age_band_tgs,
#     age_band_lower_bounds = age_band_L_bounds,
#     age_band_upper_bounds = age_band_U_bounds,
#     w_1 = w_1,
#     w_2 = w_2,
#     w_3 = w_3,
#     age_struct = age_struct,
#     out_path = out_pth_all,
#     parallel = TRUE))


# ---------------------------------------- submit all jobs


if (CLUSTER) {

  R_0_tiles <- queuer::qlapply(
    seq_along(tile_ids_2),
    burden_and_wolbachia_one_tile,
    obj,
    ids_vec = tile_ids_2,
    in_path = in_pth,
    orig_in_path = orig_in_pth,
    xx = fctr_combs,
    age_band_tgs = age_band_tgs,
    age_band_lower_bounds = age_band_L_bounds,
    age_band_upper_bounds = age_band_U_bounds,
    w_1 = w_1,
    w_2 = w_2,
    w_3 = w_3,
    age_struct = age_struct,
    out_path = out_pth_all,
    parallel = TRUE)

} else {

  R_0_tiles <- lapply(
    seq_along(tile_ids_2)[1],
    burden_and_wolbachia_one_tile,
    ids_vec = tile_ids_2,
    in_path = in_pth,
    orig_in_path = orig_in_pth,
    xx = fctr_combs,
    age_band_tgs = age_band_tgs,
    age_band_lower_bounds = age_band_L_bounds,
    age_band_upper_bounds = age_band_U_bounds,
    w_1 = w_1,
    w_2 = w_2,
    w_3 = w_3,
    age_struct = age_struct,
    out_path = out_pth_all,
    parallel = FALSE)

}

if (!CLUSTER) {
  context::parallel_cluster_stop()
}
