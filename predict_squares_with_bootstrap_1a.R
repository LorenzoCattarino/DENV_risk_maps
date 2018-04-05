

# define parameters -----------------------------------------------------------


no_samples <- 200

grid_sizes <- c(0.5, 1, 2, 5, 10)

distance_bins <- c(200, 500, 1000, 1500)
  
in_path <- file.path("output",
                     "predictions_world",
                     "bootstrap_models",
                     paste0("grid_size_", grid_sizes),
                     "FOI_boot_model",
                     "response.rds")

out_name <- "response.rds"
  
out_path <- file.path("output",
                      "predictions_world",
                      "bootstrap_models")


# load data ------------------------------------------------------------------- 


all_sqr_covariates <- readRDS(file.path("output", 
                                        "env_variables", 
                                        "all_squares_env_var_0_1667_deg_dis.rds"))

predictions_all_gr_szs <- lapply(in_path, readRDS)


# pre processing --------------------------------------------------------------


N <- nrow(all_sqr_covariates)

new_predictions <- matrix(0, nrow = N, ncol = no_samples)
  
all_distances <- all_sqr_covariates[, "distance"]
  
bin_ids <- 1 + findInterval(all_distances, distance_bins)
             
             
# start -----------------------------------------------------------------------


for (i in seq_len(N)){
  
  bin_id <- bin_ids[i] 
  
  new_predictions[i,] <- predictions_all_gr_szs[[bin_id]][i,]

}

saveRDS(new_predictions, file.path(out_path, out_name))
