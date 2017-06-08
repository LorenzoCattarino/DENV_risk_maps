options(didehpc.cluster = "fi--didemrchnb")

CLUSTER <- TRUE

my_resources <- c(
  file.path("R", "random_forest", "grid_up_foi_dataset.r"),
  file.path("R", "random_forest", "bootstrap_foi_dataset.r"),
  file.path("R", "random_forest", "make_RF_predictions.r"),
  file.path("R", "random_forest", "Exp_Max_algorithm.r"),
  file.path("R", "random_forest", "wrapper_to_Exp_Max_algorithm.r"),
  file.path("R", "utility_functions.r"))

my_pkgs <- c("ranger", "dplyr")

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
  #context::parallel_cluster_start(8, ctx)

}


# ---------------------------------------- define parameters


no_fits <- 200

pxl_dts_name <- "All_FOI_estimates_disaggreg_20km.rds"

out_md_nm_all <- paste0("boot_model_20km_cw_run_", seq_len(no_fits), ".rds")

out_prd_nm_all <- paste0("square_predictions_boot_model_20km_cw_run_", seq_len(no_fits), ".rds")

md_out_pth <- file.path("output", "model_objects")

prd_out_pth <- file.path("output", "predictions", "boot_model_20km_cw", "all_runs")

grp_flds <- c("ID_0", "ID_1", "data_id")

no_trees <- 500

min_node_size <- 20

pseudoAbs_value <- 0

all_wgt <- 1

pAbs_wgt <- 0.25

grid_size <- 5

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
  file.path("output", "foi", "All_FOI_estimates_linear_env_var.csv"),
  stringsAsFactors = FALSE) 


# ---------------------------------------- get the vector of best predictors


my_predictors <- predictor_rank$variable[1:9]


# ---------------------------------------- pre process the original foi dataset


foi_data$new_weight <- all_wgt

foi_data[foi_data$type == "pseudoAbsence", "new_weight"] <- pAbs_wgt

foi_data[foi_data$type == "pseudoAbsence", "FOI"] <- pseudoAbs_value

names(foi_data)[names(foi_data) == "FOI"] <- "o_j"

admin_dataset <- foi_data[, c(grp_flds, "o_j", "new_weight")]
  
  
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
pxl_dataset <- inner_join(pxl_dataset, foi_data[, c(grp_flds, "o_j")])


# ---------------------------------------- submit job


t <- obj$enqueue(
  exp_max_algorithm_boot(
    seq_len(no_fits)[1],
    out_model_name = out_md_nm_all,
    out_pred_name = out_prd_nm_all,
    niter = niter,
    adm_dataset = admin_dataset,
    pxl_dataset_full = pxl_dataset,
    no_trees = no_trees,
    min_node_size = min_node_size,
    my_predictors = my_predictors,
    grp_flds = grp_flds,
    model_out_path = md_out_pth,
    pred_out_path = prd_out_pth,
    gr_size = grid_size))

if (CLUSTER) {
  
  EM_alg_run <- queuer::qlapply(
    seq_len(no_fits),
    exp_max_algorithm_boot,
    obj,
    out_model_name = out_md_nm_all, 
    out_pred_name = out_prd_nm_all,
    niter = niter, 
    adm_dataset = admin_dataset, 
    pxl_dataset_full = pxl_dataset,
    no_trees = no_trees, 
    min_node_size = min_node_size, 
    my_predictors = my_predictors, 
    grp_flds = grp_flds,
    model_out_path = md_out_pth, 
    pred_out_path = prd_out_pth,
    gr_size = grid_size)
  
}else{
  
  EM_alg_run <- lapply(
    seq_len(no_fits)[1],
    exp_max_algorithm_boot,
    out_model_name = out_md_nm_all, 
    out_pred_name = out_prd_nm_all,
    niter = niter, 
    adm_dataset = admin_dataset, 
    pxl_dataset_full = pxl_dataset,
    no_trees = no_trees, 
    min_node_size = min_node_size, 
    my_predictors = my_predictors, 
    grp_flds = grp_flds,
    model_out_path = md_out_pth, 
    pred_out_path = prd_out_pth,
    gr_size = grid_size)
  
}  

if (!CLUSTER) {
  context::parallel_cluster_stop()
}
