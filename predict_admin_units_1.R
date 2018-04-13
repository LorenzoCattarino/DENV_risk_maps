# Makes predictions for all the level 1 admin units in the world.

options(didehpc.cluster = "fi--didemrchnb")

my_resources <- c(
  file.path("R", "utility_functions.r"),
  file.path("R", "random_forest", "fit_h2o_RF_and_make_predictions.R"),
  file.path("R", "prepare_datasets", "average_up.R"))

my_pkgs <- c("h2o", "dplyr")

context::context_log_start()
ctx <- context::context_save(path = "context",
                             packages = my_pkgs,
                             sources = my_resources)


# define parameters ----------------------------------------------------------- 


var_to_fit <- "FOI"

adm_level <- 2

#grp_fields <- c("ADM_0", "ADM_1")
grp_fields <- c("ADM_0", "ADM_1", "ADM_2")

#old_grp_fields <- c("ID_0", "ID_1")
old_grp_fields <- c("ID_0", "ID_1", "ID_2")
  
number_of_predictors <- 9

RF_mod_name <- "RF_obj.rds"

base_info <- c("OBJECTID", "latitude", "longitude", "population", old_grp_fields)


# define variables ------------------------------------------------------------


model_tp <- paste0(var_to_fit, "_best_model")

out_pt <- file.path("output", 
                    "predictions_world", 
                    "best_fit_models",
                    model_tp)

out_fl_nm <- paste0("response_adm", adm_level, ".rds")


# are you using the cluster? --------------------------------------------------  


context::context_load(ctx)


# load data ------------------------------------------------------------------- 


prediction_datasets <- read.csv(file.path("output", 
                                          "env_variables", 
                                          paste0("All_adm", adm_level, "_env_var.csv")))

RF_obj_path <- file.path("output",
                         "EM_algorithm",
                         "best_fit_models",
                         model_tp,
                         "optimized_model_objects")

predictor_rank <- read.csv(file.path("output", 
                                     "variable_selection", 
                                     "metropolis_hastings", 
                                     "exp_1", 
                                     "variable_rank_final_fits_exp_1.csv"),
                           stringsAsFactors = FALSE)

square_predictions <- readRDS(file.path("output",
                                        "predictions_world",
                                        "best_fit_models",
                                        model_tp,
                                        "response.rds"))
  
  
# get best predictor ---------------------------------------------------------- 


my_predictors <- predictor_rank$name[1:number_of_predictors]


# submit one job -------------------------------------------------------------- 


h2o.init()

RF_obj <- h2o.loadModel(file.path(RF_obj_path, RF_mod_name))

p_i <- make_h2o_predictions(RF_obj, prediction_datasets, my_predictors)

h2o.shutdown(prompt = FALSE)

p_i[p_i < 0] <- 0

world_adm_preds <- cbind(prediction_datasets[, base_info], adm = p_i)

world_adm_preds <- world_adm_preds %>% 
  rename_at(vars(old_grp_fields), ~ grp_fields)

average_sqr <- average_up(pxl_df = square_predictions,
                          grp_flds = grp_fields,
                          var_names = "best")

names(average_sqr)[names(average_sqr) == "best"] <- "aver_sqr"

res <- left_join(world_adm_preds, average_sqr[,-which(names(average_sqr) == "population")])

write_out_rds(res, out_pt, out_fl_nm) 
