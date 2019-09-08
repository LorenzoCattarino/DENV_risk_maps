# Calculates proportion of seronegative at some age


source(file.path("R", "utility_functions.r"))
source(file.path("R", "create_parameter_list.R"))


# define parameters -----------------------------------------------------------


extra_prms  <- list(id = 2,
                    age = c(9, 16))

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

ages <- parameters$age


# load data ------------------------------------------------------------------- 

  
sqr_preds <- readRDS(file.path("output", 
                               "predictions_world",
                               "bootstrap_models",
                               model_type,
                               "adm_1",
                               prediction_fl_nm))


# calculate proportion of seronegative at age X -------------------------------


for (i in seq_along(ages)) {
  
  age <- ages[i] 
  
  serop_var <- 100 * exp(-4 * age * sqr_preds[, col_ids]) # percentage
  
  base_info <- sqr_preds[, setdiff(names(sqr_preds), col_ids)]
  
  final_dts <- cbind(base_info, serop_var)
  
  out_fl_nm <- sprintf("p%s%s", age, ".rds")
  
  write_out_rds(final_dts, out_pt, out_fl_nm)  
  
}
