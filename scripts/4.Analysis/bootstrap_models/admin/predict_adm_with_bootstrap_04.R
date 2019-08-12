# Takes mean, median, sd and 95% CI of predictions, for each admin unit
# THIS IS FOR THE MAP!

source(file.path("R", "utility_functions.R"))
source(file.path("R", "create_parameter_list.R"))
source(file.path("R", "prepare_datasets", "calculate_mean_across_fits.R"))


# define parameters ----------------------------------------------------------- 


extra_prms <- list(id = 4) 


# define variables ------------------------------------------------------------


parameters <- create_parameter_list(extra_params = extra_prms)

vars_to_average <- c("response_endemic", 
                     "p9", 
                     "p16",
                     "p9_FP_specifA",
                     "p16_FP_specifA",
                     "p9_FP_specifB",
                     "p16_FP_specifB",
                     "C_pr_1_wolbachia_32",
                     "C_pr_1_wolbachia_16",
                     "C_pr_2_wolbachia_32",
                     "C_pr_2_wolbachia_16",
                     "C_pr_1_vaccine_8",
                     "C_pr_1_vaccine_20",
                     "C_pr_1_vaccine_32",
                     "C_pr_2_vaccine_8",
                     "C_pr_2_vaccine_20",
                     "C_pr_2_vaccine_32")#,
                     #"transformed_1_wolbachia_4",
                     #"transformed_2_wolbachia_4")

model_type <- paste0("model_", parameters$id)

col_names <- as.character(seq_len(parameters$no_samples))

in_path <- file.path("output", 
                     "predictions_world",
                     "bootstrap_models",
                     model_type,
                     "adm_1")


# -----------------------------------------------------------------------------


for (i in seq_along(vars_to_average)[8:17]){
  
  # z_values <- seq(0, 10, 2)
  
  var_to_average <- vars_to_average[i]
  
  message(var_to_average)  
  
  dat <- readRDS(file.path(in_path, paste0(var_to_average, ".rds")))
  
  dat <- as.data.frame(dat)
  
  ret <- average_boot_samples_dim2(dat[, col_names], na.rm = TRUE)
  
  anyNA_i <- apply(dat, 1, test_NA)
        
  ret[anyNA_i, ] <- NA
  
  base_info <- dat[, setdiff(names(dat), col_names)]
  
  ret2 <- cbind(base_info, ret)
  
  out_name <- paste0(var_to_average, "_mean.rds")
  
  write_out_rds(ret2, in_path, out_name)
  
}
