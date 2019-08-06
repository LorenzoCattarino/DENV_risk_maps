# Calculates seroprevalence at some age


source(file.path("R", "utility_functions.r"))
source(file.path("R", "create_parameter_list.R"))


# define parameters -----------------------------------------------------------


extra_prms  <- list(id = 4,
                    age = 16)

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

age <- parameters$age

out_fl_nm <- sprintf("p%s%s", age, ".rds")


# load data ------------------------------------------------------------------- 

  
sqr_preds <- readRDS(file.path("output", 
                               "predictions_world",
                               "bootstrap_models",
                               model_type,
                               "adm_1",
                               prediction_fl_nm))


# calculate seroprevalence at age X -------------------------------------------


serop_var <- 100 * (1 - exp(-4 * age * sqr_preds[, col_ids])) # percentage

base_info <- sqr_preds[, setdiff(names(sqr_preds), col_ids)]
  
final_dts <- cbind(base_info, serop_var)

write_out_rds(final_dts, out_pt, out_fl_nm)  

false_positive_specifA <- 0.1 * (serop_var/100)
false_positive_specifB <- 0.01 * (serop_var/100)
  
final_dtsA <- cbind(base_info, false_positive_specifA * 100)
final_dtsB <- cbind(base_info, false_positive_specifB * 100)

write_out_rds(final_dtsA, out_pt, sprintf("p%s_FP_specifA%s", age, ".rds"))
write_out_rds(final_dtsB, out_pt, sprintf("p%s_FP_specifB%s", age, ".rds"))
