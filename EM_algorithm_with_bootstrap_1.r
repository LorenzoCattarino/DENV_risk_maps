# Creates a set of bootstrap samples 

options(didehpc.cluster = "fi--didemrchnb")

CLUSTER <- FALSE

my_resources <- c(
  file.path("R", "prepare_datasets", "functions_for_creating_bootstrap_samples.r"),
  file.path("R", "prepare_datasets", "grid_up.R"),
  file.path("R", "utility_functions.r"))

my_pkgs <- c()

context::context_log_start()
ctx <- context::context_save(path = "context",
                             sources = my_resources,
                             packages = my_pkgs)


# ---------------------------------------- define parameters


no_fits <- 200

grid_size <- 5

all_wgt <- 1

pAbs_wgt <- 0.25


# ---------------------------------------- are you using the cluster? 


if (CLUSTER) {
  
  obj <- didehpc::queue_didehpc(ctx)
  
} else {
  
  context::context_load(ctx)
  context::parallel_cluster_start(8, ctx)

}


# ---------------------------------------- load data


foi_data <- read.csv(
  file.path("output", "foi", "All_FOI_estimates_linear_env_var.csv"),
  stringsAsFactors = FALSE) 


# ---------------------------------------- pre process the original foi dataset


foi_data$new_weight <- all_wgt

foi_data[foi_data$type == "pseudoAbsence", "new_weight"] <- pAbs_wgt

names(foi_data)[names(foi_data) == "ID_0"] <- "ADM_0"

names(foi_data)[names(foi_data) == "ID_1"] <- "ADM_1"


# ---------------------------------------- submit one test job


# t <- obj$enqueue(
#   grid_and_boot(
#     seq_along(boot_samples)[1],
#     a = foi_data,
#     b = grid_size))


# ---------------------------------------- submit job


if (CLUSTER) {

  get_boot_samples <- queuer::qlapply(
    seq_len(no_fits),
    grid_and_boot,
    obj,
    a = foi_data,
    b = grid_size)

} else {

  get_boot_samples <- loop(
    seq_len(no_fits),
    grid_and_boot,
    a = foi_data,
    b = grid_size,
    parallel = TRUE)

}

if(!CLUSTER){
  context::parallel_cluster_stop()
}
