# Makes foi predictions of all squares, for each tile and model fit 
# and takes the mean value of the predictions across fits 

options(didehpc.cluster = "fi--didemrchnb")

CLUSTER <- TRUE

my_resources <- c(
  file.path("R", "utility_functions.r"),
  file.path("R", "random_forest", "functions_for_making_RF_predictions.r"))
  
my_pkgs <- c("h2o")

context::context_log_start()
ctx <- context::context_save(path = "context",
                             packages = my_pkgs,
                             sources = my_resources)

context::context_load(ctx)
context::parallel_cluster_start(8, ctx)


# ---------------------------------------- define parameters


model_tp <- "boot_model_20km_cw"

no_fits <- 50


# ---------------------------------------- load data


all_sqr_covariates <- readRDS(file.path("output", "env_variables", "all_squares_env_var_0_1667_deg.rds"))

RF_obj_path <- file.path(
  "output",
  "EM_algorithm",
  model_tp,
  "optimized_model_objects")

# predicting variable rank
predictor_rank <- read.csv(
  file.path("output", 
            "variable_selection", 
            "metropolis_hastings", 
            "exp_1", 
            "variable_rank_final_fits_exp_1.csv"),
  stringsAsFactors = FALSE)


# ---------------------------------------- get best predictor


best_predictors <- predictor_rank$variable[1:9]


# ---------------------------------------- run job


foi <- wrapper_to_make_preds(
  no_fits = no_fits,
  model_in_path = RF_obj_path, 
  dataset = all_sqr_covariates, 
  predictors = best_predictors, 
  parallel = FALSE)


# ---------------------------------------- post processing 


foi[foi < 0] <- 0

mean_pred <- rowMeans(foi)
  
all_sqr_covariates$mean_pred <- mean_pred

all_sqr_covariates <- all_sqr_covariates[, c("cell", "lat.grid", "long.grid", "population", "ADM_0", "ADM_1", "ADM_2", "mean_pred")]

out_fl_nm <- "all_squares_mean_foi_0_1667_deg.rds"

out_pt <- file.path(
  "output", 
  "predictions_world",
  model_tp)


# ---------------------------------------- save mean predictions 


write_out_rds(all_sqr_covariates, out_pt, out_fl_nm)
