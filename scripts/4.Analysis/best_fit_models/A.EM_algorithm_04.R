# Runs the EM algorithm on the entire original foi dataset

library(ranger)
library(fields)
library(ggplot2)
library(weights)
library(dplyr)

source(file.path("R", "random_forest", "fit_ranger_RF_and_make_predictions.R"))
source(file.path("R", "random_forest", "exp_max_algorithm.R"))
source(file.path("R", "plotting", "functions_for_plotting_raster_maps.R"))
source(file.path("R", "plotting", "generic_scatter_plot.R"))
source(file.path("R", "prepare_datasets", "calculate_wgt_corr.R"))
source(file.path("R", "utility_functions.R"))
source(file.path("R", "create_parameter_list.R"))
source(file.path("R", "prepare_datasets", "average_up.R"))


# define parameters ----------------------------------------------------------- 


extra_prms <- list(id = 3,
                   dependent_variable = "FOI",  
                   no_predictors = 17,
                   ranger_threads = NULL,
                   id_fld = "data_id",
                   grp_flds = c("data_id", "ID_0", "ID_1"))

out_md_nm <- "RF_obj.rds"

diag_t_nm <- "diagno_table.rds"

map_nm <- "map"

tra_dts_nm <- "train_dts.rds"


# define variables ------------------------------------------------------------


parameters <- create_parameter_list(extra_params = extra_prms)

model_id <- parameters$id
  
var_to_fit <- parameters$dependent_variable

number_of_predictors <- parameters$no_predictors

model_type <- paste0("model_", model_id)

grp_flds <- parameters$grp_flds

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

covariates_dir <- parameters$covariates_dir


# load data ------------------------------------------------------------------- 


foi_data <- readRDS(file.path("output", 
                              "EM_algorithm", 
                              "best_fit_models", 
                              model_type,
                              "adm_foi_data",
                              "adm_foi_data.rds")) 

pxl_data <- readRDS(file.path("output", 
                              "EM_algorithm", 
                              "best_fit_models",
                              model_type,
                              "env_variables_and_init_pred", 
                              "covariates_and_foi_20km.rds"))

predictor_rank <- read.csv(file.path("output", 
                                     "variable_selection",
                                     covariates_dir,
                                     "predictor_rank.csv"), 
                           stringsAsFactors = FALSE)

adm_covariates <- read.csv(file.path("output",
                                     "env_variables",
                                     "All_adm1_env_var.csv"),
                           stringsAsFactors = FALSE)


# pre processing --------------------------------------------------------------


my_predictors <- predictor_rank$name[1:number_of_predictors]

names(foi_data)[names(foi_data) == var_to_fit] <- "o_j"

pxl_data_2 <- inner_join(pxl_data, foi_data[, c(grp_flds, "o_j")])


# calculate population weights

pxl_dts_grp <- pxl_data_2 %>% 
  group_by(.dots = grp_flds) %>% 
  summarise(pop_sqr_sum = sum(population))

pxl_data_3 <- left_join(pxl_data_2, pxl_dts_grp)

pxl_data_3$pop_weight <- pxl_data_3$population / pxl_data_3$pop_sqr_sum


# run job --------------------------------------------------------------------- 


EM_alg_run <- exp_max_algorithm(parms = parameters,
                                orig_dataset = foi_data,
                                pxl_dataset = pxl_data_3, 
                                my_predictors = my_predictors, 
                                RF_obj_path = RF_out_pth,
                                RF_obj_name = out_md_nm,
                                diagn_tab_path = diag_t_pth, 
                                diagn_tab_name = diag_t_nm,
                                map_path = map_pth, 
                                sct_plt_path = sct_plt_pth,
                                train_dts_path = train_dts_pth, 
                                train_dts_name = tra_dts_nm,
                                adm_dataset = adm_covariates)
