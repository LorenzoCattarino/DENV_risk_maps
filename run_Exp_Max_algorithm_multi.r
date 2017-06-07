options(didehpc.cluster = "fi--didemrchnb")

CLUSTER <- TRUE

my_resources <- c(
  file.path("R", "utility_functions.r"),
  file.path("R", "random_forest", "fit_random_forest_model.r"),
  file.path("R", "random_forest", "make_RF_predictions.r"),
  file.path("R", "random_forest", "get_1_0_point_position.r"),
  file.path("R", "random_forest", "calculate_sum_squared_errors.r"),
  file.path("R", "random_forest", "spatial_sampK_cv_rng3.r"),
  file.path("R", "random_forest", "grid_up_foi_dataset.r"),
  file.path("R", "random_forest", "bootstrap_foi_dataset.r"),
  file.path("R", "random_forest", "wrapper_to_spatial_sampK_cv_rng3.r"))

my_pkgs <- "ranger"

context::context_log_start()
ctx <- context::context_save(path = "context",
                             sources = my_resources,
                             packages = my_pkgs)


# ---------------------------------------- are you using the cluster? 


if (CLUSTER) {
  
  config <- didewin::didewin_config(template = "24Core")
  obj <- didewin::queue_didewin(ctx, config = config)
  
} else {
  
  context::context_load(ctx)
  
}


# ---------------------------------------- define parameters


pxl_dts_name <- "All_FOI_estimates_disaggreg_20km.rds"

out_md_nm <- "best_model_20km_cw.RDS"

out_prd_nm <- "square_predictions_best_model_20km_cw.RDS"

prd_out_pth <- file.path("output", "predictions", "best_model_20km_cw")

md_out_pth <- file.path("output", "model_objects")

grp_flds <- c("ID_0", "ID_1")

no_trees <- 500

min_node_size <- 20

all_wgt <- 1

pAbs_wgt <- 0.25

niter <- 35


# ---------------------------------------- load data


pxl_dataset <- readRDS(
  file.path("output", "foi", pxl_dts_name))

predictor_rank <- read.csv(
  file.path("output", 
            "variable_selection", 
            "metropolis_hastings", 
            "exp_1", 
            "variable_rank_final_fits_exp_1.csv"),
  stringsAsFactors = FALSE)

foi_data <- read.csv(
  file.path("output", "All_FOI_estimates_linear_env_var.csv"),
  stringsAsFactors = FALSE) 


# ---------------------------------------- get the vector of best predictors


my_predictors <- predictor_rank$variable[1:9]


# ---------------------------------------- pre process the original foi dataset


foi_data$new_weight <- all_wgt

foi_data[foi_data$type == "pseudoAbsence", "new_weight"] <- pAbs_wgt

names(foi_data)[names(foi_data) == "FOI"] <- "o_j"
  
  
# ---------------------------------------- pre process the prediction dataset


names(pxl_dataset)[names(pxl_dataset) == "ADM_0"] <- grp_flds[1]
names(pxl_dataset)[names(pxl_dataset) == "ADM_1"] <- grp_flds[2]

pxl_dataset[, my_predictors][pxl_dataset[, my_predictors] == 0] <- NA

na_rows <- apply(pxl_dataset, 1, anyNA)

pxl_dataset <- pxl_dataset[!na_rows, ]

px_adm <- pxl_dataset %>% group_by_(.dots = grp_flds)

adm_pop <- px_adm %>% summarise(adm_pop = sum(population))

pxl_dataset <- left_join(pxl_dataset, adm_pop)

pxl_dataset$pop_weight <- pxl_dataset$population / pxl_dataset$adm_pop

pxl_dataset$new_weight <- all_wgt

pxl_dataset[pxl_dataset$type == "pseudoAbsence", "new_weight"] <- pAbs_wgt


# ---------------------------------------- attach adm level pred to pxl dataset


# drop values in pxl_dataset which are not in o_j (PYF and HTI) 
pxl_dataset <- inner_join(pxl_dataset, o_j)


# ---------------------------------------- submit job


if (CLUSTER) {
  
queuer::qlapply(
  seq_len(no_fits),
  complete_EM_routine_multi_run)
  
}else{
  
  
}  