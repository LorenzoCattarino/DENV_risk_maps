# Fits RF to all original foi data (using fixed RF parameters) 

options(didehpc.cluster = "fi--didemrchnb")

my_resources <- c(
  file.path("R", "random_forest", "fit_ranger_RF_and_make_predictions.R"),
  file.path("R", "prepare_datasets", "set_pseudo_abs_weights.R"),
  file.path("R", "utility_functions.R"),
  file.path("R", "create_parameter_list.R"))

my_pkgs <- "ranger"

context::context_log_start()
ctx <- context::context_save(path = "context",
                             sources = my_resources,
                             packages = my_pkgs)


# define parameters ----------------------------------------------------------- 


extra_prms <- list(dependent_variable = "FOI",
                   pseudoAbs_value = -0.02,
                   no_predictors = 26,
                   foi_offset = 0.03,
                   ranger_threds = NULL)

out_name <- "all_data.rds"  

foi_dts_nm <- "All_FOI_estimates_and_predictors.csv"

extra_predictors <- NULL


# define variables ------------------------------------------------------------


parameters <- create_parameter_list(extra_params = extra_prms)

var_to_fit <- parameters$dependent_variable

foi_offset <- parameters$foi_offset
  
out_pth <- file.path("output", 
                     "EM_algorithm", 
                     "best_fit_models", 
                     paste0("model_objects_", var_to_fit, "_fit"))


# start up -------------------------------------------------------------------- 


context::context_load(ctx)


# load data ------------------------------------------------------------------- 


foi_data <- read.csv(file.path("output", "foi", foi_dts_nm),
                     stringsAsFactors = FALSE)

predictor_rank <- read.csv(file.path("output", 
                                     "variable_selection",
                                     "stepwise_v3",
                                     "predictor_rank.csv"), 
                           stringsAsFactors = FALSE)


# pre processing -------------------------------------------------------------- 


# set pseudo absence value
foi_data[foi_data$type == "pseudoAbsence", var_to_fit] <- parameters$pseudoAbs_value

# assign weights
foi_data$new_weight <- parameters$all_wgt
pAbs_wgt <- get_sat_area_wgts(foi_data, parameters)
foi_data[foi_data$type == "pseudoAbsence", "new_weight"] <- pAbs_wgt

my_predictors <- predictor_rank$name[1:parameters$no_predictors]
my_predictors <- c(my_predictors, extra_predictors)

if(var_to_fit == "FOI"){
  
  foi_data[, var_to_fit] <- foi_data[, var_to_fit] + foi_offset

}

# get training dataset (full dataset - no bootstrap)
training_dataset <- foi_data[, c(var_to_fit, my_predictors, "new_weight")]


# run job --------------------------------------------------------------------- 


RF_obj <- fit_ranger_RF(parms = parameters, 
                        dependent_variable = parameters$dependent_variable,
                        predictors = my_predictors, 
                        training_dataset = training_dataset, 
                        my_weights = "new_weight")

write_out_rds(RF_obj, out_pth, out_name)
