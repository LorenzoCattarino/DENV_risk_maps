# Take mean, median, sd and 95% CI of foi, R0 and burden measures, for each 20 km square
# THIS IS FOR THE MAP!

source(file.path("R", "utility_functions.R"))
source(file.path("R", "create_parameter_list.R"))
source(file.path("R", "prepare_datasets", "calculate_mean_across_fits.R"))


# define parameters ----------------------------------------------------------- 


extra_prms <- list(id = 4) 

#vars_to_average <- "response"
# vars_to_average <- "C_pr_2_wolbachia_16"
vars_to_average <- "C_pr_2_wolbachia_32"


# define variables ------------------------------------------------------------


parameters <- create_parameter_list(extra_params = extra_prms)

model_type <- paste0("model_", parameters$id)

col_names <- as.character(seq_len(parameters$no_samples))

in_path <- file.path("output", 
                     "predictions_world",
                     "bootstrap_models",
                     model_type)


# -----------------------------------------------------------------------------


dat <- readRDS(file.path(in_path, paste0(vars_to_average, ".rds")))

ret <- average_boot_samples_dim2(dat[, col_names])

base_info <- dat[, setdiff(names(dat), col_names)]

ret2 <- cbind(base_info, ret)

out_name <- paste0(vars_to_average, "_mean.rds")

write_out_rds(ret2, in_path, out_name)
