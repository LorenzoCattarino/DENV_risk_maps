
source(file.path("R", "burden_and_interventions", "calculate_mean_across_fits.r"))


# define parameters -----------------------------------------------------------


no_samples <- 200

grid_sizes <- c(0.5, 1, 2, 5, 10)

in_path <- file.path("output",
                     "predictions_world",
                     "bootstrap_models",
                     paste0("grid_size_", grid_sizes),
                     "FOI_boot_model",
                     "response_mean.rds")

out_name <- "response_mean.rds"

out_path <- file.path("output",
                      "predictions_world",
                      "bootstrap_models")

base_info <- c("cell", "latitude", "longitude", "population", "ADM_0", "ADM_1", "ADM_2", "sd")


# load data ------------------------------------------------------------------- 


all_sqr_covariates <- readRDS(file.path("output", 
                                        "env_variables", 
                                        "all_squares_env_var_0_1667_deg_dis.rds"))

mean_predictions_all_gr_szs <- lapply(in_path, readRDS)


# start -----------------------------------------------------------------------


n <- nrow(mean_predictions_all_gr_szs[[1]])

all_sd <- vapply(seq_along(grid_sizes), get_grid_size_sd, numeric(n), pred_ls = mean_predictions_all_gr_szs)

new_col_names <- paste0("sd_", grid_sizes)

colnames(all_sd) <- new_col_names

all_sqr_covariates <- cbind(all_sqr_covariates, all_sd)

all_sqr_covariates$distance_log <- log(all_sqr_covariates$distance)

all_sqr_covariates$sd <- 0
  
# all_sqr_covariates <- all_sqr_covariates[302510:302520,]

N <- nrow(all_sqr_covariates)

for (i in seq_len(N)){
  
  all_sqr_covariates[i, "sd"] <- approx(grid_sizes, all_sqr_covariates[i, new_col_names], xout = all_sqr_covariates[i, "distance_log"])$y

}

saveRDS(all_sqr_covariates[, base_info], file.path(out_path, out_name))
