# Calculates seroprevalence at max vaccine impact age


source(file.path("R", "utility_functions.r"))
source(file.path("R", "create_parameter_list.R"))


# define parameters -----------------------------------------------------------


extra_prms  <- list(id = 4,
                    vaccine_id = 4,
                    R0_scenario = 1)

prediction_fl_nm <- "response_endemic.rds"


# define variables ------------------------------------------------------------


parameters <- create_parameter_list(extra_params = extra_prms)

model_type <- paste0("model_", parameters$id)

out_pt <- file.path("output", 
                    "predictions_world",
                    "bootstrap_models",
                    model_type,
                    "adm_1")

col_ids <- as.character(seq_len(parameters$no_samples))

R0_scenario <- parameters$R0_scenario

vaccine_id <- parameters$vaccine_id


# load data ------------------------------------------------------------------- 


fct_c <- read.csv(file.path("output", 
                            "predictions_world", 
                            "bootstrap_models",
                            model_type,
                            "adm_1",
                            "scenario_table_vaccine.csv"))

sqr_preds <- readRDS(file.path("output", 
                               "predictions_world",
                               "bootstrap_models",
                               model_type,
                               "adm_1",
                               prediction_fl_nm))

max_age <- readRDS(file.path(out_pt, "I_num_1_max_age_vaccine_4.rds"))
  
  
# calculate seroprevalence at age X -------------------------------------------


burden_measure <- toupper(substr(fct_c[vaccine_id, "burden_measure"], 1, 1))

vars_to_average <- sprintf("%s_num_%s_max_age_vaccine_%s", burden_measure, R0_scenario, vaccine_id)

out_fl_nm <- sprintf("pvacc_optimal_age_%s_R0%s_%s%s", burden_measure, R0_scenario, vaccine_id, ".rds")

serop_var <- 100 * (1 - exp(-4 * max_age[, col_ids] * sqr_preds[, col_ids])) # percentage

base_info <- sqr_preds[, setdiff(names(sqr_preds), col_ids)]

final_dts <- cbind(base_info, serop_var)

write_out_rds(final_dts, out_pt, out_fl_nm)  
