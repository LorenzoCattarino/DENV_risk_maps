# Runs the EM algorithm on the entire original foi dataset

options(didehpc.cluster = "fi--didemrchnb")

CLUSTER <- TRUE

my_resources <- c(
  file.path("R", "random_forest", "fit_ranger_RF_and_make_predictions.R"),
  file.path("R", "prepare_datasets", "set_pseudo_abs_weights.R"),
  file.path("R", "random_forest", "exp_max_algorithm.R"),
  file.path("R", "plotting", "quick_raster_map.R"),
  file.path("R", "plotting", "generic_scatter_plot.R"),
  file.path("R", "prepare_datasets", "calculate_wgt_corr.R"),
  file.path("R", "utility_functions.R"),
  file.path("R", "plotting", "functions_for_plotting_square_level_maps.R"))  

my_pkgs <- c("ranger", "dplyr", "fields", "ggplot2", "weights", "colorRamps", "raster")

context::context_log_start()
ctx <- context::context_save(path = "context",
                             sources = my_resources,
                             packages = my_pkgs)


# define parameters ----------------------------------------------------------- 


parameters <- list(
  resample_grid_size = 20,
  dependent_variable = "FOI",
  shape_1 = 0,
  shape_2 = 5,
  shape_3 = 1.6e6,
  pseudoAbs_value = -0.02,
  foi_offset = 0.03,
  no_trees = 500,
  min_node_size = 20,
  all_wgt = 1,
  wgt_limits = c(1, 500),
  EM_iter = 10,
  no_predictors = 26) 

grp_flds <- c("ID_0", "ID_1", "data_id")

out_md_nm <- "RF_obj.rds"

diag_t_nm <- "diagno_table.rds"

map_nm <- "map"

tra_dts_nm <- "train_dts.rds"

foi_dts_nm <- "All_FOI_estimates_and_predictors_2.csv"

pxl_dts_name <- "covariates_and_foi_20km.rds"

model_type_tag <- "_best_model_5"

extra_predictors <- NULL


# define variables ------------------------------------------------------------


var_to_fit <- parameters$dependent_variable

foi_offset <- parameters$foi_offset

number_of_predictors <- parameters$no_predictors

pseudoAbsence_value <- parameters$pseudoAbs_value

all_wgt <- parameters$all_wgt

model_type <- paste0(var_to_fit, model_type_tag)

res <- (1 / 120) * parameters$resample_grid_size

RF_out_pth <- file.path("output", 
                        "EM_algorithm", 
                        "best_fit_models",
                        model_type,
                        "optimized_model_objects")

diag_t_pth <- file.path("output", 
                        "EM_algorithm", 
                        "best_fit_models",
                        model_type,
                        "diagnostics")

train_dts_pth <- file.path("output",
                           "EM_algorithm",
                           "best_fit_models",
                           model_type,
                           "training_datasets")

map_pth <- file.path("figures", 
                     "EM_algorithm", 
                     "best_fit_models",
                     model_type,
                     "maps")

sct_plt_pth <- file.path("figures", 
                         "EM_algorithm", 
                         "best_fit_models",
                         model_type,
                         "iteration_fits")


# are you using the cluster? --------------------------------------------------  


if (CLUSTER) {
  
  config <- didehpc::didehpc_config(template = "16Core")
  obj <- didehpc::queue_didehpc(ctx, config = config)

} else {
  
  context::context_load(ctx)

}


# load data ------------------------------------------------------------------- 


foi_data <- read.csv(file.path("output", "foi", foi_dts_nm),
                     stringsAsFactors = FALSE) 

pxl_data <- readRDS(file.path("output", 
                              "EM_algorithm", 
                              "best_fit_models",
                              paste0("env_variables_", var_to_fit, "_fit"), 
                              pxl_dts_name))

predictor_rank <- read.csv(file.path("output", 
                                     "variable_selection",
                                     "stepwise",
                                     "predictor_rank.csv"), 
                           stringsAsFactors = FALSE)

adm_covariates <- read.csv(file.path("output",
                                     "env_variables",
                                     "All_adm1_env_var.csv"),
                           stringsAsFactors = FALSE)


# pre processing --------------------------------------------------------------


# pxl_data$NightTemp_const_term <- ifelse(pxl_data$NightTemp_const_term > 6000, 6000, pxl_data$NightTemp_const_term)
# pxl_data$log_pop_den <- ifelse(pxl_data$log_pop_den > 0.6, 0.6, pxl_data$log_pop_den)
# pxl_data[pxl_data$square == 229595, "population"] <- 10000
# grid_size <- (1 / 120) * 20
# sqr_area_km <- (grid_size * 111.32)^2
# pxl_data[pxl_data$square == 229595, "pop_den"] <- pxl_data[pxl_data$square == 229595, "population"] / sqr_area_km
# pxl_data[pxl_data$square == 229595, "log_pop_den"] <- log(1 + pxl_data[pxl_data$square == 229595, "pop_den"])

names(foi_data)[names(foi_data) == var_to_fit] <- "o_j"

foi_data[foi_data$type == "pseudoAbsence", "o_j"] <- pseudoAbsence_value

foi_data$new_weight <- all_wgt
pAbs_wgt <- get_sat_area_wgts(foi_data, parameters)
foi_data[foi_data$type == "pseudoAbsence", "new_weight"] <- pAbs_wgt

if(var_to_fit == "FOI"){
  
  foi_data[, "o_j"] <- foi_data[, "o_j"] + foi_offset
  
}

adm_covariates <- adm_covariates[!duplicated(adm_covariates[, c("ID_0", "ID_1")]), ]

pxl_data <- inner_join(pxl_data, foi_data[, c(grp_flds, "type", "new_weight")])


# fix serology new_weights ----------------------------------------------------


pxl_data[pxl_data$type == "serology", "new_weight"] <- 0

sero_points <- foi_data[foi_data$type == "serology", ]

pxl_data$lat.int <- round(pxl_data$latitude / res)
pxl_data$long.int <- round(pxl_data$longitude / res)

sero_points$lat.int <- round(sero_points$latitude / res)
sero_points$long.int <- round(sero_points$longitude / res)

sero_points$cell <- 0
sero_points$no_square <- 0

for (i in seq_len(nrow(sero_points))){

  sero_long <- sero_points[i, "long.int"]
  sero_lat <- sero_points[i, "lat.int"]

  matches <- pxl_data$type == "serology" & pxl_data$lat.int == sero_lat & pxl_data$long.int == sero_long

  if(sum(matches) != 0){

    message(i)

    cell_id <- which(matches == TRUE)[1]
    sero_points[i, "cell"] <- cell_id
    pxl_data[cell_id, "new_weight"] <- 1

  } else {

    sero_points[i, "no_square"] <- 1

  }

}

missing_square <- sero_points[sero_points$no_square == 1, ]
# write.csv(missing_square, file.path("output", "EM_algorithm", "missing_squares_for_orginal_datapoints.csv"), row.names = FALSE)

sero_pxl_no_dup <- pxl_data$type == "serology" & pxl_data$new_weight == 1

pxl_data_2 <- pxl_data[!sero_pxl_no_dup, ]

sero_pxl_dup <- pxl_data[sero_points$cell, ]

sero_pxl_dup$data_id <- sero_points$data_id

pxl_data_3 <- rbind(pxl_data_2, sero_pxl_dup)
# test <- cbind(sero_pxl_dup[c("ID_0", "ID_1")], sero_points[,c("ID_0", "ID_1", "latitude", "longitude")])

pxl_data_3 <- inner_join(pxl_data_3, foi_data[, c(grp_flds, "o_j")])

write_out_rds(pxl_data_3, 
              file.path("output",
                        "EM_algorithm",
                        "best_fit_models",
                        "env_variables_FOI_fit"), 
              "covariates_and_foi_20km_2.rds")


# get pop weights -------------------------------------------------------------


pxl_dts_grp <- pxl_data_3 %>% group_by_(.dots = grp_flds) 

aa <- pxl_dts_grp %>% summarise(pop_sqr_sum = sum(population))
# ncells <- pxl_dts_grp %>% summarise(n_sqr = n())

pxl_data_3 <- left_join(pxl_data_3, aa)
# pxl_data <- left_join(pxl_data, ncells)

pxl_data_3$pop_weight <- pxl_data_3$population / pxl_data_3$pop_sqr_sum
# pxl_data$pop_weight <- 1 / pxl_data$n_sqr


# -----------------------------------------------------------------------------


my_predictors <- predictor_rank$name[1:number_of_predictors]
my_predictors <- c(my_predictors, extra_predictors)


# submit job ------------------------------------------------------------------ 


if (CLUSTER) {
  
  EM_alg_run <- obj$enqueue(
    exp_max_algorithm(
      parms = parameters,
      orig_dataset = foi_data,
      pxl_dataset = pxl_data_3,
      my_predictors = my_predictors, 
      grp_flds = grp_flds,
      var_to_fit = var_to_fit,
      RF_obj_path = RF_out_pth,
      RF_obj_name = out_md_nm,
      diagn_tab_path = diag_t_pth, 
      diagn_tab_name = diag_t_nm,
      map_path = map_pth, 
      map_name = map_nm,
      sct_plt_path = sct_plt_pth,
      train_dts_path = train_dts_pth, 
      train_dts_name = tra_dts_nm,
      adm_dataset = adm_covariates))
  
} else {
  
  EM_alg_run <- exp_max_algorithm(
    parms = parameters,
    orig_dataset = foi_data,
    pxl_dataset = pxl_data_3, 
    my_predictors = my_predictors, 
    grp_flds = grp_flds,
    var_to_fit = var_to_fit,
    RF_obj_path = RF_out_pth,
    RF_obj_name = out_md_nm,
    diagn_tab_path = diag_t_pth, 
    diagn_tab_name = diag_t_nm,
    map_path = map_pth, 
    map_name = map_nm,
    sct_plt_path = sct_plt_pth,
    train_dts_path = train_dts_pth, 
    train_dts_name = tra_dts_nm,
    adm_dataset = adm_covariates)
  
}

if (!CLUSTER) {
  context::parallel_cluster_stop()
}
