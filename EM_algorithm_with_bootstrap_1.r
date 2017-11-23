# Creates a set of bootstrap samples 

options(didehpc.cluster = "fi--didemrchnb")

my_resources <- c(
  file.path("R", "prepare_datasets", "functions_for_creating_bootstrap_samples.r"),
  file.path("R", "prepare_datasets", "grid_up.R"),
  file.path("R", "utility_functions.r"))

my_pkgs <- c()

context::context_log_start()
ctx <- context::context_save(path = "context",
                             sources = my_resources,
                             packages = my_pkgs)

context::context_load(ctx)
context::parallel_cluster_start(8, ctx)


# ---------------------------------------- define parameters


no_fits <- 200

grid_size <- 5

out_fl_nm <- "bootstrap_samples.rds"

out_pt <- file.path("output", "EM_algorithm")


# ---------------------------------------- load data


foi_data <- read.csv(
  file.path("output", "foi", "All_FOI_estimates_linear_env_var_area.csv"),
  stringsAsFactors = FALSE) 


# ---------------------------------------- pre process the original foi dataset


names(foi_data)[names(foi_data) == "ID_0"] <- "ADM_0"

names(foi_data)[names(foi_data) == "ID_1"] <- "ADM_1"


# ---------------------------------------- submit jobs


boot_samples <- loop(
  seq_len(no_fits),
  grid_and_boot,
  a = foi_data,
  b = grid_size,
  parallel = TRUE)


# ---------------------------------------- save


write_out_rds(boot_samples, out_pt, out_fl_nm)


# ---------------------------------------- stop cluster


context::parallel_cluster_stop()

