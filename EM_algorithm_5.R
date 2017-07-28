# Runs the EM algorithm on the entire original foi dataset

options(didehpc.cluster = "fi--didemrchnb")

CLUSTER <- TRUE

my_resources <- c(
  file.path("R", "utility_functions.R"),
  file.path("R", "random_forest", "Exp_Max_algorithm.R"),
  file.path("R", "random_forest", "fit_h2o_random_forest_model.r"),
  file.path("R", "random_forest", "make_h2o_RF_predictions.r"),
  file.path("R", "random_forest", "quick_raster_map.r"))  

my_pkgs <- c("h2o", "dplyr", "fields")

context::context_log_start()
ctx <- context::context_save(path = "context",
                             sources = my_resources,
                             packages = my_pkgs)


# ---------------------------------------- are you using the cluster? 


if (CLUSTER) {
  
  config <- didehpc::didehpc_config(template = "24Core")
  obj <- didehpc::queue_didehpc(ctx, config = config)

} else {
  
  context::context_load(ctx)

}


# ---------------------------------------- define parameters


dependent_variable <- "o_j"

no_trees <- 500

min_node_size <- 20

all_wgt <- 1

pAbs_wgt <- 0.25

niter <- 200

grp_flds <- c("ID_0", "ID_1", "data_id")

pxl_dts_name <- "All_FOI_estimates_disaggreg_20km.rds"

out_md_nm <- "best_model_20km_cw.rds"

md_out_pth <- file.path("output", "EM_algorithm", "optimized_model_objects")

diag_t_pth <- file.path("output", "EM_algorithm", "diagnostics", "best_model_20km_cw")

diag_t_nm <- "diagno_table.rds"

map_pth <- file.path("figures", "EM_algorithm", "best_model_20km_cw", "maps")

map_nm <- "map"

sq_pred_pth <- file.path("output", "EM_algorithm", "square_predictions", "best_model_20km_cw")

sq_pred_nm <- "dd_debug.rds"

wgt_ftcr <- 1
#wgt_ftcr <- 1 / 10000


# ---------------------------------------- load data


pxl_dataset <- readRDS(
  file.path("output", "EM_algorithm", "env_variables_foi", pxl_dts_name))

predictor_rank <- read.csv(
  file.path("output", 
            "variable_selection", 
            "metropolis_hastings", 
            "exp_1", 
            "variable_rank_final_fits_exp_1.csv"),
  stringsAsFactors = FALSE)

foi_data <- read.csv(
  file.path("output", "foi", "All_FOI_estimates_linear_env_var.csv"),
  stringsAsFactors = FALSE) 


# ---------------------------------------- get the vector of best predictors


my_predictors <- predictor_rank$variable[1:9]


# ---------------------------------------- pre process the original foi dataset


foi_data$new_weight <- all_wgt

foi_data[foi_data$type == "pseudoAbsence", "new_weight"] <- pAbs_wgt

names(foi_data)[names(foi_data) == "FOI"] <- dependent_variable


# ---------------------------------------- pre process the prediction dataset


names(pxl_dataset)[names(pxl_dataset) == "ADM_0"] <- grp_flds[1]
names(pxl_dataset)[names(pxl_dataset) == "ADM_1"] <- grp_flds[2]

pxl_dataset$pop_weight <- pxl_dataset$population / pxl_dataset$adm_pop

pxl_dataset$new_weight <- all_wgt

pxl_dataset[pxl_dataset$type == "pseudoAbsence", "new_weight"] <- pAbs_wgt


# ---------------------------------------- attach adm level pred to pxl dataset


# drop values in pxl_dataset which are not in o_j (PYF and HTI) 
pxl_dataset <- inner_join(pxl_dataset, foi_data[, c(grp_flds, dependent_variable)])


# ---------------------------------------- submit job


if (CLUSTER) {
  
  EM_alg_run <- obj$enqueue(
    exp_max_algorithm(
      niter = niter, 
      adm_dataset = foi_data,
      pxl_dataset = pxl_dataset,
      pxl_dataset_full = pxl_dataset,
      no_trees = no_trees, 
      min_node_size = min_node_size,
      my_predictors = my_predictors, 
      grp_flds = grp_flds,
      RF_obj_path = md_out_pth,
      RF_obj_name = out_md_nm,
      diagn_tab_path = diag_t_pth, 
      diagn_tab_name = diag_t_nm,
      map_path = map_pth, 
      map_name = map_nm,
      sq_pr_path = sq_pred_pth, 
      sq_pr_name = sq_pred_nm,
      wgt_factor = wgt_ftcr))
  
} else {
  
  EM_alg_run <- exp_max_algorithm(
    niter = niter, 
    adm_dataset = foi_data,
    pxl_dataset = pxl_dataset, 
    pxl_dataset_full = pxl_dataset,
    no_trees = no_trees, 
    min_node_size = min_node_size,
    my_predictors = my_predictors, 
    grp_flds = grp_flds,
    RF_obj_path = md_out_pth,
    RF_obj_name = out_md_nm,
    diagn_tab_path = diag_t_pth, 
    diagn_tab_name = diag_t_nm,
    map_path = map_pth, 
    map_name = map_nm,
    sq_pr_path = sq_pred_pth, 
    sq_pr_name = sq_pred_nm,
    wgt_factor = wgt_ftcr)

}

if (!CLUSTER) {
  context:::stop_parallel_cluster()
}
