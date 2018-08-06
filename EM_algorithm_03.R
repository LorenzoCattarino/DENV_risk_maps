# Estimate foi for each 20 km square of the dataset disaggregated from the entire original foi dataset

options(didehpc.cluster = "fi--didemrchnb")

my_resources <- c(
  file.path("R", "random_forest", "fit_ranger_RF_and_make_predictions.R"),
  file.path("R", "utility_functions.R"))  

my_pkgs <- "ranger"

context::context_log_start()
ctx <- context::context_save(path = "context",
                             sources = my_resources,
                             packages = my_pkgs)


# define parameters ----------------------------------------------------------- 


parameters <- list(
  dependent_variable = "FOI",
  no_predictors = 26)   

aggr_dts_name <- "env_vars_20km.rds"

out_fl_nm <- "covariates_and_foi_20km.rds"

model_obj_nm <- "all_data.rds"

extra_predictors <- NULL


# define variables ------------------------------------------------------------


out_pth <- file.path("output", 
                     "EM_algorithm", 
                     "best_fit_models",
                     paste0("env_variables_", parameters$dependent_variable, "_fit"))
  
  
# start up -------------------------------------------------------------------- 


context::context_load(ctx)


# load data -------------------------------------------------------------------


RF_obj <- readRDS(file.path("output",
                            "EM_algorithm",
                            "best_fit_models",
                            paste0("model_objects_", parameters$dependent_variable, "_fit"),
                            model_obj_nm))

pxl_data <- readRDS(file.path("output", 
                              "EM_algorithm",
                              "best_fit_models",
                              "env_variables", 
                              aggr_dts_name))

predictor_rank <- read.csv(file.path("output", 
                                     "variable_selection",
                                     "stepwise",
                                     "predictor_rank.csv"), 
                           stringsAsFactors = FALSE)


# pre processing --------------------------------------------------------------


my_predictors <- predictor_rank$name[1:parameters$no_predictors]
my_predictors <- c(my_predictors, extra_predictors)

# pxl_data$log_pop_den <- ifelse(pxl_data$log_pop_den > 0.6, 0.6, pxl_data$log_pop_den)
# pxl_data[pxl_data$square == 229595, "population"] <- 10000
# grid_size <- (1 / 120) * 20
# sqr_area_km <- (grid_size * 111.32)^2
# pxl_data[pxl_data$square == 229595, "pop_den"] <- pxl_data[pxl_data$square == 229595, "population"] / sqr_area_km
# pxl_data[pxl_data$square == 229595, "log_pop_den"] <- log(1 + pxl_data[pxl_data$square == 229595, "pop_den"]) 


# submit job ------------------------------------------------------------------ 


p_i <- make_ranger_predictions(
  mod_obj = RF_obj, 
  dataset = pxl_data, 
  sel_preds = my_predictors)

pxl_data$p_i <- p_i

write_out_rds(pxl_data, out_pth, out_fl_nm)
