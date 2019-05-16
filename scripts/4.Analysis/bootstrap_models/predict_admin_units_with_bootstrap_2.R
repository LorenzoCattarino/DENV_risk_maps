# Calculates seroprevalence at age 9 (`p9` variable).


source(file.path("R", "utility_functions.r"))


# define parameters -----------------------------------------------------------


parameters <- list(
  dependent_variable = "FOI",
  grid_size = 1,
  no_samples = 200)   

age <- 9

out_fl_nm <- "p9.rds"

prediction_fl_nm <- "response.rds"


# define variables ------------------------------------------------------------


model_type <- paste0(parameters$dependent_variable, "_boot_model")

my_dir <- paste0("grid_size_", parameters$grid_size)

out_pt <- file.path("output", 
                    "predictions_world",
                    "bootstrap_models",
                    my_dir,
                    model_type)

col_ids <- as.character(seq_len(parameters$no_samples))


# load data ------------------------------------------------------------------- 


sqr_preds <- readRDS(file.path("output", 
                               "predictions_world",
                               "bootstrap_models",
                               my_dir,
                               model_type,
                               prediction_fl_nm))


# calculate p9 ----------------------------------------------------------------


p9 <- 100 * (1 - exp(-4 * age * sqr_preds[, col_ids]))

base_info <- sqr_preds[, setdiff(names(sqr_preds), col_ids)]

final_dts <- cbind(base_info, p9)

write_out_rds(final_dts, out_pt, out_fl_nm)  
