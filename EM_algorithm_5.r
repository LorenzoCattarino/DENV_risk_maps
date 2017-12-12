# Runs the EM algorithm on the entire original foi dataset

options(didehpc.cluster = "fi--didemrchnb")

CLUSTER <- TRUE

my_resources <- c(
  file.path("R", "utility_functions.R"),
  file.path("R", "random_forest", "functions_for_fitting_h2o_RF_and_making_predictions.r"),
  file.path("R", "random_forest", "Exp_Max_algorithm.R"),
  file.path("R", "plotting", "quick_raster_map.r"),
  file.path("R", "plotting", "generic_scatter_plot.r"))  

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


model_type <- "best_model_20km_3c"

var_to_fit <- "R0_1"

pseudoAbsence_value <- 0.5

niter <- 10

no_trees <- 500

min_node_size <- 20

all_wgt <- 1

wgt_limits <- c(1, 1000)

grp_flds <- c("ID_0", "ID_1", "data_id")

pxl_dts_name <- "covariates_and_foi_20km.rds"

out_md_nm <- "RF_obj.rds"

diag_t_nm <- "diagno_table.rds"

map_nm <- "map"


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
  file.path("output", "foi", "All_FOI_estimates_linear_env_var_area.csv"),
  stringsAsFactors = FALSE) 

pxl_dataset <- readRDS(
  file.path("output", "EM_algorithm", paste0("env_variables_", var_to_fit, "_fit"), pxl_dts_name))

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
  stringsAsFactors = FALSE)


# ---------------------------------------- get the vector of best predictors


my_predictors <- predictor_rank$variable[1:9]

#my_predictors <- c(my_predictors, "RFE_const_term", "pop_den")


# ---------------------------------------- pre process the original foi data set


names(foi_data)[names(foi_data) == var_to_fit] <- "o_j"

foi_data[foi_data$type == "pseudoAbsence", "o_j"] <- pseudoAbsence_value

foi_data$new_weight <- all_wgt
pAbs_wgt <- get_area_scaled_wgts(foi_data, wgt_limits)
foi_data[foi_data$type == "pseudoAbsence", "new_weight"] <- pAbs_wgt


# ---------------------------------------- pre process the admin data set


adm_dataset <- adm_dataset[!duplicated(adm_dataset[, c("ID_0", "ID_1")]), ]


# ---------------------------------------- pre process the square data set


names(pxl_dataset)[names(pxl_dataset) == "ADM_0"] <- grp_flds[1]
names(pxl_dataset)[names(pxl_dataset) == "ADM_1"] <- grp_flds[2]

pxl_dts_grp <- pxl_dataset %>% group_by_(.dots = grp_flds) 

aa <- pxl_dts_grp %>% summarise(pop_sqr_sum = sum(population))

pxl_dataset <- left_join(pxl_dataset, aa)

pxl_dataset$pop_weight <- pxl_dataset$population / pxl_dataset$pop_sqr_sum


# ---------------------------------------- attach original data and weigths to square dataset


pxl_dataset <- inner_join(pxl_dataset, foi_data[, c(grp_flds, "o_j", "new_weight")])


# ---------------------------------------- submit job


if (CLUSTER) {
  
  EM_alg_run <- obj$enqueue(
    exp_max_algorithm(
      niter = niter, 
      orig_dataset = foi_data,
      pxl_dataset = pxl_dataset,
      pxl_dataset_full = pxl_dataset,
      l_f = pseudoAbsence_value,
      my_predictors = my_predictors, 
      grp_flds = grp_flds,
      RF_obj_path = RF_out_pth,
      RF_obj_name = out_md_nm,
      diagn_tab_path = diag_t_pth, 
      diagn_tab_name = diag_t_nm,
      map_path = map_pth, 
      map_name = map_nm,
      sct_plt_path = sct_plt_pth,
      var_to_fit = var_to_fit,
      adm_dataset = adm_dataset))
  
} else {
  
  EM_alg_run <- exp_max_algorithm(
    niter = niter, 
    orig_dataset = foi_data,
    pxl_dataset = pxl_dataset, 
    pxl_dataset_full = pxl_dataset,
    l_f = pseudoAbsence_value,
    my_predictors = my_predictors, 
    grp_flds = grp_flds,
    RF_obj_path = RF_out_pth,
    RF_obj_name = out_md_nm,
    diagn_tab_path = diag_t_pth, 
    diagn_tab_name = diag_t_nm,
    map_path = map_pth, 
    map_name = map_nm,
    sct_plt_path = sct_plt_pth,
    var_to_fit = var_to_fit,
    adm_dataset = adm_dataset)

}

if (!CLUSTER) {
  context:::stop_parallel_cluster()
}
