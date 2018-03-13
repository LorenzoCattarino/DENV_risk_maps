# Creates a set of bootstrap samples 

options(didehpc.cluster = "fi--didemrchnb")

my_resources <- c(
  file.path("R", "prepare_datasets", "functions_for_creating_bootstrap_samples.r"),
  file.path("R", "prepare_datasets", "set_pseudo_abs_weights.R"),
  file.path("R", "prepare_datasets", "grid_up.R"),
  file.path("R", "utility_functions.r"))

context::context_log_start()
ctx <- context::context_save(path = "context",
                             sources = my_resources)

context::context_load(ctx)
context::parallel_cluster_start(8, ctx)


# define parameters ----------------------------------------------------------- 


no_fits <- 200

grid_size <- 1

out_fl_nm <- "bootstrap_samples.rds"

all_wgt <- 1

wgt_limits <- c(1, 500)


# define variables ------------------------------------------------------------


my_dir <- paste0("grid_size_", grid_size)

out_pt <- file.path("output", "EM_algorithm", "bootstrap_models", my_dir)


# load data ------------------------------------------------------------------- 


foi_data <- read.csv(
  file.path("output", "foi", "All_FOI_estimates_linear_env_var_area.csv"),
  stringsAsFactors = FALSE) 


# pre process the original foi dataset ---------------------------------------- 


names(foi_data)[names(foi_data) == "ID_0"] <- "ADM_0"

names(foi_data)[names(foi_data) == "ID_1"] <- "ADM_1"

foi_data$new_weight <- all_wgt

pAbs_wgt <- get_area_scaled_wgts(foi_data, wgt_limits)

foi_data[foi_data$type == "pseudoAbsence", "new_weight"] <- pAbs_wgt


# submit jobs ----------------------------------------------------------------- 


boot_samples <- loop(
  seq_len(no_fits),
  grid_and_boot,
  a = foi_data,
  b = grid_size,
  parallel = TRUE)


# save ------------------------------------------------------------------------ 


write_out_rds(boot_samples, out_pt, out_fl_nm)


# stop cluster ---------------------------------------------------------------- 


context::parallel_cluster_stop()

