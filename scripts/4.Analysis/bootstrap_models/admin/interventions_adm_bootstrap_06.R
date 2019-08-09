# Takes mean, median, sd and 95% CI of 

# 1) optimal vaccine impact age, for each admin unit, across bootstrap samples
# 2) seroprevalence at optimal vaccine impact age

source(file.path("R", "utility_functions.R"))
source(file.path("R", "create_parameter_list.R"))
source(file.path("R", "prepare_datasets", "calculate_mean_across_fits.R"))


# define parameters ----------------------------------------------------------- 


extra_prms <- list(id = 4,
                   vaccine_id = c(28, 32, 36),
                   R0_scenario = c(1, 2),
                   age = c(TRUE, FALSE)) 


# define variables ------------------------------------------------------------


parameters <- create_parameter_list(extra_params = extra_prms)

model_type <- paste0("model_", parameters$id)

in_path <- file.path("output", 
                     "predictions_world",
                     "bootstrap_models",
                     model_type,
                     "adm_1")

col_ids <- as.character(seq_len(parameters$no_samples))

vaccine_ids <- parameters$vaccine_id

R0_scenarios <- parameters$R0_scenario

ages <- parameters$age


# load data # -----------------------------------------------------------------


fct_c <- read.csv(file.path("output", 
                            "predictions_world", 
                            "bootstrap_models",
                            model_type,
                            "adm_1",
                            "scenario_table_vaccine.csv"))


# -----------------------------------------------------------------------------


for (i in seq_along(vaccine_ids)) {
  
  for(j in seq_along(R0_scenarios)) {
    
    for (k in seq_along(ages)) {
      
      vaccine_id <- vaccine_ids[i]
      R0_scenario <- R0_scenarios[j]
      age <- ages[k]
      
      burden_measure <- toupper(substr(fct_c[fct_c$id == vaccine_id, "burden_measure"], 1, 1))
      
      var_to_average <- sprintf("%s_num_%s_max_age_vaccine_%s", burden_measure, R0_scenario, vaccine_id)
      
      if(!age){
        
        var_to_average <- sprintf("%s_%s", "p", var_to_average)
        
      }
      
      dat <- readRDS(file.path(in_path, paste0(var_to_average, ".rds")))
      
      ret <- average_boot_samples_dim2(dat[, col_ids])
      
      base_info <- dat[, setdiff(names(dat), col_ids)]
      
      ret2 <- cbind(base_info, ret)
      
      out_name <- paste0(var_to_average, "_mean.rds")
      
      write_out_rds(ret2, in_path, out_name)
      
    }
    
  }
  
}
