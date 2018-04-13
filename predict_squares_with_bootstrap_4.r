# Calculates seroprevalence at age 9 (`p9` variable).


source(file.path("R", "utility_functions.r"))


# define parameters -----------------------------------------------------------


parameters <- list(
  grid_size = 1,
  no_trees = 500,
  min_node_size = 20,
  pseudoAbs_value = -0.02,
  all_wgt = 1,
  wgt_limits = c(1, 500),
  no_samples = 10,
  EM_iter = 10,
  no_predictors = 9)   

var_to_fit <- "FOI"

model_tp <- "R0_1_boot_model" 

scenario_id <- 1
  
out_fl_nm <- "p9_all_squares.rds"

out_pt <- file.path(
  "output", 
  "predictions_world",
  model_tp)

in_dts_tag <- "all_squares"


# get name of file to load ----------------------------------------------------


var_to_fit <- sub("_boot.*", "", model_tp)
  
if(var_to_fit == "FOI") {
  
  mean_pred_fl_nm <- paste0(var_to_fit, "_", in_dts_tag, ".rds")

} else {
  
  mean_pred_fl_nm <- paste0("FOI_r", "_", in_dts_tag, "_", scenario_id, ".rds")

}


# load data ------------------------------------------------------------------- 

  
all_sqr_foi <- readRDS(
  file.path(
    "output", 
    "predictions_world",
    model_tp,
    mean_pred_fl_nm))

p9 <- 100 *(1 - exp(-36 * all_sqr_foi))

write_out_rds(p9, out_pt, out_fl_nm)  
