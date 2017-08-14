# Runs the EM algorithm on the entire original foi dataset

options(didehpc.cluster = "fi--didemrchnb")

CLUSTER <- TRUE

my_resources <- c(
  file.path("R", "utility_functions.R"),
  file.path("R", "random_forest", "Exp_Max_algorithm.R"),
  file.path("R", "random_forest", "fit_h2o_random_forest_model.r"),
  file.path("R", "random_forest", "make_h2o_RF_predictions.r"),
  file.path("R", "random_forest", "quick_raster_map.r"),
  file.path("R", "random_forest", "get_lm_equation.r"),
  file.path("R", "generic_scatter_plot.r"))  

my_pkgs <- c("h2o", "dplyr", "fields", "ggplot2")

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


model_type <- "best_model_20km_cw"

niter <- 10

dependent_variable <- "o_j"

no_trees <- 500

min_node_size <- 20

all_wgt <- 1

pAbs_wgt <- 0.25

grp_flds <- c("ID_0", "ID_1", "data_id")

pxl_dts_name <- "All_FOI_estimates_disaggreg_20km.rds"

out_md_nm <- "best_model_20km_cw.rds"

diag_t_nm <- "diagno_table.rds"

map_nm <- "map"

sq_pred_nm <- "dd_debug.rds"


# ========================================
# 
# output paths - IMPORTANT!
# 
# ========================================


RF_out_pth <- file.path(
  "output", 
  "EM_algorithm", 
  model_type,
  "optimized_model_objects")

diag_t_pth <- file.path(
  "output", 
  "EM_algorithm", 
  model_type,
  "diagnostics")

sq_pred_pth <- file.path(
  "output", 
  "EM_algorithm", 
  model_type,
  "square_predictions")

map_pth <- file.path(
  "figures", 
  "EM_algorithm", 
  model_type,
  "maps")

sct_plt_pth <- file.path(
  "figures", 
  "EM_algorithm", 
  model_type,
  "iteration_fits")


# ---------------------------------------- load data


foi_data <- read.csv(
  file.path("output", "foi", "All_FOI_estimates_linear_env_var.csv"),
  stringsAsFactors = FALSE) 

pxl_dataset <- readRDS(
  file.path("output", "EM_algorithm", "env_variables_foi", pxl_dts_name))

predictor_rank <- read.csv(
  file.path("output", 
            "variable_selection", 
            "metropolis_hastings", 
            "exp_1", 
            "variable_rank_final_fits_exp_1.csv"),
  stringsAsFactors = FALSE)

adm_dataset <- read.csv(  
  file.path("output",
            "env_variables",
            "All_adm1_env_var.csv"),
  header = TRUE,
  sep = ",", 
  stringsAsFactors = FALSE)


# ---------------------------------------- get the vector of best predictors


my_predictors <- predictor_rank$variable[1:9]


# ---------------------------------------- pre process the original foi data set


foi_data$new_weight <- all_wgt

foi_data[foi_data$type == "pseudoAbsence", "new_weight"] <- pAbs_wgt

names(foi_data)[names(foi_data) == "FOI"] <- dependent_variable


# ---------------------------------------- pre process the admin data set


adm_dataset <- adm_dataset[!duplicated(adm_dataset[, c("ID_0", "ID_1")]), ]


# ---------------------------------------- pre process the square data set


names(pxl_dataset)[names(pxl_dataset) == "ADM_0"] <- grp_flds[1]
names(pxl_dataset)[names(pxl_dataset) == "ADM_1"] <- grp_flds[2]

#pxl_dataset[pxl_dataset$population > 0, "population"] <- 1

pxl_dts_grp <- pxl_dataset %>% group_by_(.dots = grp_flds) 

aa <- pxl_dts_grp %>% summarise(pop_sqr_sum = sum(population))

pxl_dataset <- left_join(pxl_dataset, aa)

pxl_dataset$pop_weight <- pxl_dataset$population / pxl_dataset$pop_sqr_sum

pxl_dataset$new_weight <- all_wgt

pxl_dataset[pxl_dataset$type == "pseudoAbsence", "new_weight"] <- pAbs_wgt


# ---------------------------------------- attach original data to square dataset


pxl_dataset <- inner_join(pxl_dataset, foi_data[, c(grp_flds, dependent_variable)])


# ---------------------------------------- submit job


if (CLUSTER) {
  
  EM_alg_run <- obj$enqueue(
    exp_max_algorithm(
      niter = niter, 
      orig_dataset = foi_data,
      pxl_dataset = pxl_dataset,
      pxl_dataset_full = pxl_dataset,
      no_trees = no_trees, 
      min_node_size = min_node_size,
      my_predictors = my_predictors, 
      grp_flds = grp_flds,
      RF_obj_path = RF_out_pth,
      RF_obj_name = out_md_nm,
      diagn_tab_path = diag_t_pth, 
      diagn_tab_name = diag_t_nm,
      map_path = map_pth, 
      map_name = map_nm,
      sq_pr_path = sq_pred_pth, 
      sq_pr_name = sq_pred_nm,
      sct_plt_path = sct_plt_pth,
      adm_dataset = adm_dataset))
  
} else {
  
  EM_alg_run <- exp_max_algorithm(
    niter = niter, 
    orig_dataset = foi_data,
    pxl_dataset = pxl_dataset, 
    pxl_dataset_full = pxl_dataset,
    no_trees = no_trees, 
    min_node_size = min_node_size,
    my_predictors = my_predictors, 
    grp_flds = grp_flds,
    RF_obj_path = RF_out_pth,
    RF_obj_name = out_md_nm,
    diagn_tab_path = diag_t_pth, 
    diagn_tab_name = diag_t_nm,
    map_path = map_pth, 
    map_name = map_nm,
    sq_pr_path = sq_pred_pth, 
    sq_pr_name = sq_pred_nm,
    sct_plt_path = sct_plt_pth,
    adm_dataset = adm_dataset)

}

if (!CLUSTER) {
  context:::stop_parallel_cluster()
}
