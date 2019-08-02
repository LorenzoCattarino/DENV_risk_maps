# Average sqr predictions to admin unit level  

options(didehpc.cluster = "fi--didemrchnb")

my_resources <- c(
  file.path("R", "utility_functions.R"),
  file.path("R", "create_parameter_list.R"),
  file.path("R", "prepare_datasets", "average_up.R"))

my_pkgs <- "dplyr"

context::context_log_start()
ctx <- context::context_save(path = "context",
                             packages = my_pkgs,
                             sources = my_resources)

context::context_load(ctx)


# define parameters ----------------------------------------------------------- 


extra_prms <- list(id = 4,
                   R0_scenario = 2,
                   baseline_scenario_ids = 4,
                   dependent_variable = "FOI",
                   base_info = c("cell", 
                                 "latitude", 
                                 "longitude", 
                                 "population", 
                                 "ID_0", 
                                 "ID_1", 
                                 "ID_2"),
                   grp_fields = c("ID_0", "ID_1")) 

input_fl_name <- "transformed"


# define variables ------------------------------------------------------------


parameters <- create_parameter_list(extra_params = extra_prms)

var_to_fit <- parameters$dependent_variable

model_type <- paste0("model_", parameters$id)

R0_scenario <- parameters$R0_scenario

baseline_scenario_ids <- parameters$baseline_scenario_ids
  
grp_fields <- parameters$grp_fields

out_pt <- file.path("output", 
                    "predictions_world", 
                    "bootstrap_models",
                    model_type,
                    "adm_1")


# load data -------------------------------------------------------------------


fl_nm <- sprintf("%s_%s_%s_%s%s", 
                 input_fl_name, 
                 R0_scenario, 
                 "wolbachia", 
                 baseline_scenario_ids, 
                 ".rds")
  
sqr_preds <- readRDS(file.path("output", 
                               "predictions_world",
                               "bootstrap_models",
                               model_type,
                               fl_nm))


# average up the sqr predictions ----------------------------------------------


sqr_preds <- as.data.frame(sqr_preds)
  
col_ids <- as.character(seq_len(parameters$no_samples))
  
test <- lapply(col_ids, multi_col_average_up, sqr_preds, grp_fields)

test2 <- lapply(test, "[[", 3)

all_adm_pred <- do.call("cbind", test2)

all_adm_pred <- cbind(test[[1]][, c(grp_fields, "population")], all_adm_pred)

write_out_rds(all_adm_pred, out_pt, fl_nm)  
