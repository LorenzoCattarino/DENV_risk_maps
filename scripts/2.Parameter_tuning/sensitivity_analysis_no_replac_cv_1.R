rm(list = ls())

# devtools::install_github(c(
#    "dide-tools/context",
#    "richfitz/queuer",
#    "dide-tools/didewin"), force = TRUE, upgrade = FALSE))

my_resources <- c(
  file.path("R", "random_forest", "spatial_sampK_cv_rng2_stratif_pred.R"),
  file.path("R", "random_forest", "wrapper_for_running_multi_factor_RF.R"), 
  file.path("R", "convert_df_to_list.R"),
  file.path("R", "random_forest", "map_admin_foi.R"),
  file.path("R", "random_forest", "RF_preds_vs_obs_stratified_plot.R")
)

my_pkgs <- c("ranger", "weights", "maptools", "dplyr", "colorRamps", "reshape2")

CLUSTER <- FALSE


# ------------------------------------- Are you using the cluster?


if(CLUSTER) {
  
  # Load packages
  library(context)
  library(queuer)
  library(didewin)
  
  my_workdir <- "Q:/dengue_risk_mapping"
  
  didewin::didewin_config_global(cluster = "fi--didemrchnb", workdir = my_workdir)
  
  root <- file.path(my_workdir, "context")
  
  ctx <- context::context_save(packages = my_pkgs,
                               sources = my_resources,
                               root = root,
                               package_sources = context::package_sources(github = "richfitz/approximate"))
  
  obj <- didewin::queue_didewin(ctx, sync = "R")
  
}else{
  
  # Load packages
  sapply(my_pkgs, library, character.only = TRUE)
  
  # Load functions 
  sapply(my_resources, source)
  
}


# ---------------------------------------- Load data


# Load dataset for RF training/validating
dengue_dataset <- read.csv(file.path("data", "foi", "All_FOI_estimates_linear_env_var.csv")) 

# Load datasets for predictions
adm1_dataset <- read.csv(file.path("data", "env_variables", "All_adm1_env_var_and_age.csv"))
adm2_dataset <- read.csv(file.path("data", "env_variables", "All_adm2_env_var_and_age.csv"))

# Load shapefiles
country_border_shp_fl <- readShapePoly(file.path("data", "shapefiles", "gadm28_levels.shp", "gadm28_adm0.shp"))
adm1_shp <- readShapePoly(file.path("data", "shapefiles", "gadm28_levels.shp", "gadm28_adm1.shp"))
adm2_shp <- readShapePoly(file.path("data", "shapefiles", "gadm28_levels.shp", "gadm28_adm2.shp"))

# load best predictors
predictor_rank <- read.csv((file.path("output", 
                                      "dengue_dataset", 
                                      "variable_selection", 
                                      "metropolis_hastings", 
                                      "exp_1", 
                                      "variable_rank_final_fits_exp_1.csv")),
                           stringsAsFactors = FALSE)


# ---------------------------------------- Pre-processing 


# Remove NA and outliers
dengue_dataset <- dengue_dataset[!is.na(dengue_dataset$FOI), ]
dengue_dataset <- subset(dengue_dataset, country != "French Polynesia" & country != "Haiti")

# Sort data in decreasing FOI value
dengue_dataset <- dengue_dataset[order(dengue_dataset$FOI, decreasing = TRUE), ]

# Remove Antarctica
country_border_shp_fl <- country_border_shp_fl[!country_border_shp_fl@data$NAME_ENGLI == "Antarctica", ]

list_of_datasets <- vector("list", length = 3)
list_of_datasets[[1]] <- dengue_dataset
list_of_datasets[[2]] <- adm1_dataset
list_of_datasets[[3]] <- adm2_dataset

list_of_shp <- vector("list", length = 3)
list_of_shp[[1]] <- country_border_shp_fl 
list_of_shp[[2]] <- adm1_shp 
list_of_shp[[3]] <- adm2_shp   


# ---------------------------------------- Set up simulation framework


# Get all combinations of factors for model fitting
factor_combinations <- expand.grid(ID.exp = 7, 
                                   grid.size = 5, 
                                   cell.fraction = c(0.5, 0.7), 
                                   train.fraction = c(0.5, 0.7, 1), 
                                   pseudoAbs.value = 0,
                                   pseudoAbs.prop = 1,
                                   weights = 1,
                                   pred_tag = "MH_selected_predictors", 
                                   adm = 1)

additiona_runs <- data.frame(
  ID.exp = 7, 
  grid.size = 5, 
  cell.fraction = 1, 
  train.fraction = c(0.5, 0.7), 
  pseudoAbs.value = 0,
  pseudoAbs.prop = 1,
  weights = 1,
  pred_tag = "MH_selected_predictors", 
  adm = 1)
  
factor_combinations <- rbind(factor_combinations, additiona_runs)

# Rearrange order of varying factors
factor_combinations <- factor_combinations[order(factor_combinations$adm), ]

# Add run ID field
factor_combinations <- cbind(ID.run = seq_len(nrow(factor_combinations)), factor_combinations)

# Get all the combinations of predictors
altitude_var_names <- "altitude"
fourier_transform_elements <- c("const_term",	"Re0",	"Im0",	"Re1",	"Im1")
FTs_data_names <- c("DayTemp", "EVI", "MIR", "NightTemp", "RFE")
LandCover_var_names <- c("lct1_2012001_0",	"lct1_2012001_1",	"lct1_2012001_2",	"lct1_2012001_3",	
                         "lct1_2012001_4",	"lct1_2012001_5",	"lct1_2012001_6",	"lct1_2012001_7",	
                         "lct1_2012001_8",	"lct1_2012001_9",	"lct1_2012001_10",	"lct1_2012001_11",
                         "lct1_2012001_12",	"lct1_2012001_13",	"lct1_2012001_14",	"lct1_2012001_15",
                         "lct1_2012001_16",	"lct1_2012001_254",	"lct1_2012001_255")
all_predictors <- c(altitude_var_names, 
                    apply(expand.grid(fourier_transform_elements, FTs_data_names), 1, function(x) paste(x[2],x[1], sep="_")),
                    LandCover_var_names)

best_10_predictors <- c(
  "DayTemp_Re1", 
  "MIR_Re1",
  "altitude",
  "lct1_2012001_12",
  "lct1_2012001_2",
  "RFE_Im1",
  "NightTemp_const_term",
  "lct1_2012001_9",
  "NightTemp_Im0",
  "NightTemp_Re0")

best_10_predictors_no_LC <- c(
  "DayTemp_const_term",
  "MIR_Im0",
  "RFE_Re0",
  "NightTemp_Im0",
  "DayTemp_Re1",
  "MIR_Re1",
  "altitude",
  "RFE_Im1",
  "NightTemp_const_term",
  "NightTemp_Re0")

arbitrary_10_predictors <- c(
  "NightTemp_const_term",
  "DayTemp_Re0",
  "EVI_const_term",
  "NightTemp_Im0",
  "NightTemp_Re0",
  "RFE_const_term",
  "DayTemp_const_term",
  "EVI_Re0",
  "RFE_Re1",
  "MIR_Im1",
  "DayTemp_Im0")

best_6_predictors <- c(
  "lct1_2012001_2",
  "RFE_Im1",
  "NightTemp_const_term",
  "lct1_2012001_9",
  "NightTemp_Im0",
  "NightTemp_Re0")

best_6_predictors_no_LC <- c(
  "DayTemp_Re1",
  "MIR_Re1",
  "altitude",
  "RFE_Im1",
  "NightTemp_const_term",
  "NightTemp_Re0")

arbitrary_6_predictors <- c(
  "NightTemp_const_term",
  "DayTemp_Re0",
  "EVI_const_term",
  "NightTemp_Im0",
  "NightTemp_Re0",
  "RFE_const_term")

# get the vector of best predictors (from MH variable selection routine)
MH_selected_predictors <- predictor_rank$variable[1:9]

predictor_combination_list <- list(
  all_predictors = all_predictors,
  best_10_predictors = best_10_predictors,
  best_10_predictors_no_LC = best_10_predictors_no_LC,
  arbitrary_10_predictors = arbitrary_10_predictors,
  best_6_predictors = best_6_predictors,
  best_6_predictors_no_LC = best_6_predictors_no_LC,
  arbitrary_6_predictors = arbitrary_6_predictors,
  MH_selected_predictors = MH_selected_predictors
)


factor_combinations_list <- df_to_list (x = factor_combinations, use_names = TRUE)


# ------------------------------------- Run RF fit


if (CLUSTER) {
  
  RF_run <- queuer::qlapply(factor_combinations_list, 
                            RF_multi_factor_wrapper, 
                            obj,
                            list_of_data = list_of_datasets, 
                            all_predictor_combs = predictor_combination_list,
                            dependent_variable = "FOI",
                            output_folder = "predictions",
                            plot_predictions = FALSE,   
                            output_predictions = FALSE,
                            list_of_shp_files = list_of_shp,
                            factor_combs = factor_combinations,
                            timeout = 0)
  
} else {
  
  RF_run <- lapply(factor_combinations_list,
                   RF_multi_factor_wrapper,
                   list_of_data = list_of_datasets, 
                   all_predictor_combs = predictor_combination_list,
                   dependent_variable = "FOI",
                   output_folder = "predictions",
                   plot_predictions = FALSE,   
                   output_predictions = FALSE,
                   list_of_shp_files = list_of_shp,
                   factor_combs = factor_combinations)
  
}
