# Calculates p9 variable.


source(file.path("R", "utility_functions.r"))


# ---------------------------------------- define parameters


model_tp <- "FOI_boot_model" 

out_fl_nm <- "p9_all_squares.rds"

out_pt <- file.path(
  "output", 
  "predictions_world",
  model_tp)


# ---------------------------------------- load data


all_sqr_foi <- readRDS(
  file.path(
    "output", 
    "predictions_world",
    model_tp,
    "FOI_all_squares.rds"))

p9 <- 100 *(1 - exp(-36 * all_sqr_foi))


write_out_rds(p9, out_pt, out_fl_nm)  
