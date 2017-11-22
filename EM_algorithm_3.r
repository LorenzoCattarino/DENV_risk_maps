# Fits RF to all original foi data (using fixed RF parameters) 

options(didehpc.cluster = "fi--didemrchnb")

my_resources <- c(
  file.path("R", "utility_functions.r"),
  file.path("R", "random_forest", "functions_for_fitting_h2o_RF_and_making_predictions.r"))

my_pkgs <- "h2o"

context::context_log_start()
ctx <- context::context_save(path = "context",
                             sources = my_resources,
                             packages = my_pkgs)


# ---------------------------------------- define parameters


dependent_variable <- "R0_3"

pseudoAbsence_value <- 0.5

no_trees <- 500

min_node_size <- 20

all_wgt <- 1

pAbs_wgt <- 1

pAbs_wgt_AUS <- 20

out_path <- file.path("output", "EM_algorithm", paste0("model_objects_", dependent_variable, "_fit"))

out_name <- "all_data.rds"   


# ---------------------------------------- start up


context::context_load(ctx)


# ---------------------------------------- load data


# load FOI dataset
foi_data <- read.csv(
  file.path("output", "foi", "All_FOI_estimates_linear_env_var_area.csv"),
  stringsAsFactors = FALSE)

# predicting variable rank
predictor_rank <- read.csv(
  file.path("output", 
            "variable_selection", 
            "metropolis_hastings", 
            "exp_1", 
            "variable_rank_final_fits_exp_1.csv"),
  stringsAsFactors = FALSE)


# ---------------------------------------- get the vector of best predictors


my_predictors <- predictor_rank$variable[1:9]

my_predictors <- c(my_predictors, "RFE_const_term", "pop_den")


# ---------------------------------------- pre process the original foi dataset


# set pseudo absence value
foi_data[foi_data$type == "pseudoAbsence", dependent_variable] <- pseudoAbsence_value

# assign weights
foi_data$new_weight <- all_wgt
foi_data[foi_data$type == "pseudoAbsence", "new_weight"] <- pAbs_wgt
foi_data[foi_data$type == "pseudoAbsence" & foi_data$ID_0 == 15, "new_weight"] <- pAbs_wgt_AUS


# ---------------------------------------- create objects needed for run


# get training dataset (full dataset - no bootstrap)
training_dataset <- foi_data[, c(dependent_variable, my_predictors, "new_weight")]


# ---------------------------------------- # run job


h2o.init()

RF_obj <- fit_h2o_RF(dependent_variable = dependent_variable, 
                     predictors = my_predictors, 
                     training_dataset = training_dataset, 
                     no_trees = no_trees, 
                     min_node_size = min_node_size,
                     my_weights = "new_weight",
                     model_nm = out_name)

h2o.saveModel(RF_obj, out_path, force = TRUE)

h2o.shutdown(prompt = FALSE)
