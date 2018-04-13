
# define parameters -----------------------------------------------------------


out_name <- "response_CV.rds"

out_path <- file.path("output",
                      "predictions_world",
                      "bootstrap_models")


# load data ------------------------------------------------------------------- 


best_prediction <- readRDS(file.path("output", 
                                     "predictions_world", 
                                     "best_fit_models",
                                     "FOI_best_model",
                                     "response.rds"))
  
interpolated_sd <- readRDS(file.path("output", 
                                     "predictions_world", 
                                     "bootstrap_models",
                                     "response_mean.rds"))


# start -----------------------------------------------------------------------


interpolated_sd$CV <- interpolated_sd$sd / best_prediction$best
  
interpolated_sd$CV[is.na(interpolated_sd$CV)] <- 0
interpolated_sd$CV[!is.finite(interpolated_sd$CV)] <- 0

saveRDS(interpolated_sd, file.path(out_path, out_name))
