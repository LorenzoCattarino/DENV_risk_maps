# Calculates R0 values (4 different assumptions) from each foi value, 
# for each tile and model fit

options(didehpc.cluster = "fi--didemrchnb")

CLUSTER <- FALSE

my_resources <- c(
  file.path("R", "utility_functions.r"),
  file.path("R", "burden_and_interventions", "get_age_band_bounds.R"),
  file.path("R", "burden_and_interventions", "wrapper_to_get_R0.r"),  
  file.path("R", "burden_and_interventions", "calculate_probabilities_and_R0.r"),
  file.path("R", "burden_and_interventions", "calculate_infection_probability_and_number.r"),
  file.path("R", "burden_and_interventions", "calculate_average_infect_probab.r"),
  file.path("R", "burden_and_interventions", "calculate_R0.r"),
  file.path("R", "burden_and_interventions", "load_tile_get_burden_and_wolbachia.r"))

my_pkgs <- c("data.table", "approximate")

context::context_log_start()
ctx <- context::context_save(path = "context",
                             sources = my_resources,
                             packages = my_pkgs,
                             package_sources = context::package_sources(github = "richfitz/approximate"))


# ---------------------------------------- define parameters


model_tp <- "boot_model_20km_cw" 

out_fl_nm <- "pred_0_1667_deg_long.rds"

NA_tile_fl_name <- "NA_pixel_tiles_20km.txt"

sympt_weights <- c(0.45, 0.85, 0.15)

phi_vals <- c(1, 2, 3, 4)

sf_vals <- c(1, 0.3)


# ---------------------------------------- define variables 


in_pth <- file.path(
  "output", 
  "predictions_world", 
  model_tp,
  "tile_sets_0_1667_deg")

out_pth <- file.path(
  "output", 
  "burden",
  model_tp)


# ---------------------------------------- are you using the cluster?


if (CLUSTER) {
  
  config <- didehpc::didehpc_config(template = "12and16Core")
  obj <- didehpc::queue_didehpc(ctx)
  
} else {
  
  context::context_load(ctx)
  #context::start_parallel_cluster(8, ctx)
  
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


# ---------------------------------------- extract info from age structure 


age_band_tgs <- grep("band", names(age_struct), value = TRUE)

age_band_bnds <- get_age_band_bounds(age_band_tgs)

age_band_L_bounds <- age_band_bnds[, 1]

age_band_U_bounds <- age_band_bnds[, 2] + 1


# ---------------------------------------- pre process age structure


zero_age_str_countries <- apply(age_struct[, age_band_tgs], 1, sum) == 0

age_struct <- age_struct[!zero_age_str_countries, ]


# ---------------------------------------- get tile ids


# get tile ids
tile_ids <- tile_summary$tile.id

NA_pixel_tile_ids <- NA_pixel_tiles$tile_id

tile_ids_2 <- tile_ids[!tile_ids %in% NA_pixel_tile_ids]  


# ---------------------------------------- create combinations of R0 assumptions and wolbachia effects


fctr_combs <- expand.grid(
  phi = phi_vals,
  scaling_factor = sf_vals)

fctr_combs <- fctr_combs[order(fctr_combs$phi,
                               -fctr_combs$scaling_factor), ]

fctr_combs <- cbind(ID_run = seq_len(nrow(fctr_combs)), fctr_combs)

fctr_combs_ls <- df_to_list(x = fctr_combs, use_names = TRUE)


# ---------------------------------------- submit one job 


t <- obj$enqueue(load_tile_get_burden_and_wolbachia(
  seq_along(tile_ids_2)[1], 
  ids_vec = tile_ids_2, 
  in_path = in_pth, 
  model_in_path, parallel,
  base_info, var_names, no_fits,
  average, model_type, out_path, xx))


# ---------------------------------------- submit all jobs


if (CLUSTER) {
  
  
} else {
  
  R_0_tiles <- lapply(
    seq_along(tile_ids_2)[1],
    wrapper_to_load_tile_dataset,
    ids_vec = tile_ids_2,
    in_path = in_pth,
    out_path = out_pth,
    my_fun = wrapper_to_make_preds,
    predictors = best_predictors,
    model_in_path = RF_obj_path,
    parallel = FALSE,
    base_info = bs_inf,
    var_names = var_names,
    no_fits = no_fits,
    average = TRUE,
    model_type = model_tp)
  
}
