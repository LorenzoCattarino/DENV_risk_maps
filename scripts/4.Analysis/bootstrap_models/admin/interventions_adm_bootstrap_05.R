# Calculates seroprevalence at max vaccine impact age


source(file.path("R", "utility_functions.r"))
source(file.path("R", "create_parameter_list.R"))


# define parameters -----------------------------------------------------------


# only use the burden impact of vaccine estimated using the MEAN vaccine impact 
# so only consider some of the vaccine scenarios


extra_prms  <- list(id = 4,
                    vaccine_id = c(28, 32, 36),  
                    R0_scenario = c(1, 2))

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

R0_scenarios <- parameters$R0_scenario

vaccine_ids <- parameters$vaccine_id

  
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

  
# calculate seroprevalence at age X -------------------------------------------


for (i in seq_along(vaccine_ids)) {
  
  for(j in seq_along(R0_scenarios)) {
    
    vaccine_id <- vaccine_ids[i]
    
    R0_scenario <- R0_scenarios[j]
      
    burden_measure <- toupper(substr(fct_c[fct_c$id == vaccine_id, "burden_measure"], 1, 1))
    
    optimal_age_fl_nm <- sprintf("%s_num_%s_max_age_vaccine_%s%s", burden_measure, R0_scenario, vaccine_id, ".rds")
    
    optimal_age <- readRDS(file.path(out_pt, optimal_age_fl_nm))
    
    vars_to_average <- sprintf("%s_num_%s_max_age_vaccine_%s", burden_measure, R0_scenario, vaccine_id)
    
    out_fl_nm <- sprintf("p_%s%s", vars_to_average, ".rds")
    
    serop_var <- 100 * (1 - exp(-4 * optimal_age[, col_ids] * sqr_preds[, col_ids])) # percentage
    
    base_info <- sqr_preds[, setdiff(names(sqr_preds), col_ids)]
    
    final_dts <- cbind(base_info, serop_var)
    
    write_out_rds(final_dts, out_pt, out_fl_nm)  
    
  }
  
}
