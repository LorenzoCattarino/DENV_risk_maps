# Takes mean, median, sd and 95% CI of max vaccine impact age, for each admin unit
# THIS IS FOR THE MAP!

source(file.path("R", "utility_functions.R"))
source(file.path("R", "create_parameter_list.R"))
source(file.path("R", "prepare_datasets", "calculate_mean_across_fits.R"))


# define parameters ----------------------------------------------------------- 


extra_prms <- list(id = 4,
                   vaccine_id = 4,
                   R0_scenario = 2) 


# define variables ------------------------------------------------------------


parameters <- create_parameter_list(extra_params = extra_prms)

model_type <- paste0("model_", parameters$id)

R0_scenario <- parameters$R0_scenario

vaccine_id <- parameters$vaccine_id

col_names <- as.character(seq_len(parameters$no_samples))

in_path <- file.path("output", 
                     "predictions_world",
                     "bootstrap_models",
                     model_type,
                     "adm_1")


# load data # -----------------------------------------------------------------


fct_c <- read.csv(file.path("output", 
                            "predictions_world", 
                            "bootstrap_models",
                            model_type,
                            "adm_1",
                            "scenario_table_vaccine.csv"))


# -----------------------------------------------------------------------------


burden_measure_s <- fct_c[fct_c$id == vaccine_id, "burden_measure"]

burden_measure <- toupper(substr(burden_measure_s, 1, 1))

vars_to_average <- sprintf("%s_num_%s_max_age_vaccine_%s", burden_measure, R0_scenario, vaccine_id)

dat <- readRDS(file.path(in_path, paste0(vars_to_average, ".rds")))

ret <- average_boot_samples_dim2(dat[, col_names])

base_info <- dat[, setdiff(names(dat), col_names)]

ret2 <- cbind(base_info, ret)

out_name <- paste0(vars_to_average, "_mean.rds")

write_out_rds(ret2, in_path, out_name)
