# 1) Average pixel level impact to admin unit 1
# 2) Take mean, median, sd and 95% CI of predictions
# 3) Keep only adm values within endemic adm 1 (those where pseudo absences were sampled)

library(dplyr)

source(file.path("R", "utility_functions.R"))
source(file.path("R", "create_parameter_list.R"))
source(file.path("R", "prepare_datasets", "average_up.R"))
source(file.path("R", "prepare_datasets", "calculate_mean_across_fits.R"))


# define parameters ----------------------------------------------------------- 


extra_prms <- list(id = 2,
                   grp_fields = c("ID_0", "ID_1"),
                   ID_0_to_remove = c(1, 69, 171, 122, 200, 224, 226, 235, 236, 244, 246),
                   vars_to_average = c("C_pr_1_wolbachia_16",
                                       "C_pr_1_wolbachia_32",
                                       "C_pr_2_wolbachia_16",
                                       "C_pr_2_wolbachia_32",
                                       "C_pr_1_vaccine_8",
                                       "C_pr_1_vaccine_20",
                                       "C_pr_2_vaccine_8",
                                       "C_pr_2_vaccine_20"))
                   

# load data -------------------------------------------------------------------


endemic_ID_0_ID_1 <- read.csv(file.path("output", 
                                        "datasets", 
                                        "dengue_endemic_ID_0_ID_1.csv"),
                              stringsAsFactors = FALSE)


# define variables ------------------------------------------------------------


parameters <- create_parameter_list(extra_params = extra_prms)

model_type <- paste0("model_", parameters$id)

col_names <- as.character(seq_len(parameters$no_samples))

grp_fields <- parameters$grp_fields

vars_to_average <- parameters$vars_to_average

ID_0_to_remove <- parameters$ID_0_to_remove

in_path <- file.path("output", 
                     "predictions_world",
                     "bootstrap_models",
                     model_type)


# pre processing --------------------------------------------------------------


endemic_ID_0_ID_1 <- endemic_ID_0_ID_1[!endemic_ID_0_ID_1$ID_0 %in% ID_0_to_remove,]


# -----------------------------------------------------------------------------


for (i in seq_along(vars_to_average)) {
  
  one_nm <- vars_to_average[i] 
  
  message(one_nm)
  
  dat <- readRDS(file.path(in_path, paste0(one_nm, ".rds")))
  
  test <- lapply(col_names, multi_col_average_up, dat, grp_fields)
    
  test2 <- lapply(test, "[[", 3)
  
  all_adm_pred <- do.call("cbind", test2)  
  
  all_adm_pred <- as.data.frame(all_adm_pred)
  
  names(all_adm_pred) <- col_names
  
  ret <- average_boot_samples_dim2(all_adm_pred[, col_names], na.rm = TRUE)
  
  ret <- cbind(test[[1]][, c(grp_fields, "population")], ret)
  
  ret_2 <- inner_join(ret, endemic_ID_0_ID_1)
  
  out_name <- paste0(one_nm, "_adm_mean.rds")
  
  write_out_rds(ret_2, in_path, out_name)
  
}
