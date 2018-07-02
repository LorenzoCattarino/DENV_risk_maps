# Runs the EM algorithm on the entire original foi dataset

options(didehpc.cluster = "fi--didemrchnb")

CLUSTER <- TRUE

my_resources <- c(
  file.path("R", "random_forest", "fit_h2o_RF_and_make_predictions.R"),
  file.path("R", "prepare_datasets", "set_pseudo_abs_weights.R"),
  file.path("R", "random_forest", "exp_max_algorithm.R"),
  file.path("R", "plotting", "quick_raster_map.R"),
  file.path("R", "plotting", "generic_scatter_plot.R"),
  file.path("R", "prepare_datasets", "calculate_wgt_corr.R"),
  file.path("R", "utility_functions.R"))  

my_pkgs <- c("h2o", "dplyr", "fields", "ggplot2", "weights", "colorRamps")

context::context_log_start()
ctx <- context::context_save(path = "context",
                             sources = my_resources,
                             packages = my_pkgs)


# define parameters ----------------------------------------------------------- 


parameters <- list(
  dependent_variable = "FOI",
  pseudoAbs_value = -0.02,
  all_wgt = 1,
  wgt_limits = c(1, 500),
  no_trees = 500,
  min_node_size = 20,
  EM_iter = 10,
  no_predictors = 9)   

grp_flds <- c("ID_0", "ID_1", "data_id")

out_md_nm <- "RF_obj.rds"

diag_t_nm <- "diagno_table.rds"

map_nm <- "map"

tra_dts_nm <- "train_dts.rds"

out_fl_nm <- "square_predictions_all_data.rds"

foi_dts_nm <- "All_FOI_estimates_linear_env_var_area_salje.csv"

pxl_dts_name <- "covariates_and_foi_20km_6.rds"

model_type_tag <- "_best_model_6"

extra_predictors <- "log_pop_den"


# define variables ------------------------------------------------------------


var_to_fit <- parameters$dependent_variable

number_of_predictors <- parameters$no_predictors

pseudoAbsence_value <- parameters$pseudoAbs_value

all_wgt <- parameters$all_wgt

wgt_limits <- parameters$wgt_limits

model_type <- paste0(var_to_fit, model_type_tag)

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

out_pt <- file.path("output", "EM_algorithm", "best_fit_models", model_type)


# are you using the cluster? --------------------------------------------------  


if (CLUSTER) {
  
  config <- didehpc::didehpc_config(template = "24Core")
  obj <- didehpc::queue_didehpc(ctx, config = config)

} else {
  
  context::context_load(ctx)

}


# load data ------------------------------------------------------------------- 


foi_data <- read.csv(file.path("output", "foi", foi_dts_nm),
                     stringsAsFactors = FALSE) 

pxl_dataset <- readRDS(file.path("output", 
                                 "EM_algorithm", 
                                 "best_fit_models",
                                 paste0("env_variables_", var_to_fit, "_fit"), 
                                 pxl_dts_name))

predictor_rank <- read.csv(file.path("output", 
                                     "variable_selection", 
                                     "metropolis_hastings", 
                                     "exp_1", 
                                     "variable_rank_final_fits_exp_1.csv"),
                           stringsAsFactors = FALSE)

adm_dataset <- read.csv(file.path("output",
                                  "env_variables",
                                  "All_adm1_env_var.csv"),
                        stringsAsFactors = FALSE)


# pre processing --------------------------------------------------------------


names(foi_data)[names(foi_data) == var_to_fit] <- "o_j"

foi_data[foi_data$type == "pseudoAbsence", "o_j"] <- pseudoAbsence_value

foi_data$new_weight <- all_wgt
pAbs_wgt <- get_area_scaled_wgts(foi_data, wgt_limits)
foi_data[foi_data$type == "pseudoAbsence", "new_weight"] <- pAbs_wgt

adm_dataset <- adm_dataset[!duplicated(adm_dataset[, c("ID_0", "ID_1")]), ]

pxl_dts_grp <- pxl_dataset %>% group_by_(.dots = grp_flds) 

aa <- pxl_dts_grp %>% summarise(pop_sqr_sum = sum(population))

pxl_dataset <- left_join(pxl_dataset, aa)

pxl_dataset$pop_weight <- pxl_dataset$population / pxl_dataset$pop_sqr_sum

pxl_dataset <- inner_join(pxl_dataset, foi_data[, c(grp_flds, "o_j", "new_weight")])

my_predictors <- predictor_rank$name[1:number_of_predictors]
my_predictors <- c(my_predictors, extra_predictors)


# submit job ------------------------------------------------------------------ 


if (CLUSTER) {
  
  EM_alg_run <- obj$enqueue(
    exp_max_algorithm(
      parms = parameters,
      orig_dataset = foi_data,
      pxl_dataset = pxl_dataset,
      pxl_dataset_full = pxl_dataset,
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
      train_dts_path = train_dts_pth, 
      train_dts_name = tra_dts_nm,
      adm_dataset = adm_dataset))
  
} else {
  
  EM_alg_run <- exp_max_algorithm(
    parms = parameters,
    orig_dataset = foi_data,
    pxl_dataset = pxl_dataset, 
    pxl_dataset_full = pxl_dataset,
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
    train_dts_path = train_dts_pth, 
    train_dts_name = tra_dts_nm,
    adm_dataset = adm_dataset)

  write_out_rds(EM_alg_run, out_pt, out_fl_nm)
  
}

if (!CLUSTER) {
  context::parallel_cluster_stop()
}
