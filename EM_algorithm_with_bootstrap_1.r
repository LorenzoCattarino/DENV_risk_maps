# Creates a set of bootstrap samples 

options(didehpc.cluster = "fi--didemrchnb")

CLUSTER <- TRUE

my_resources <- c(
  file.path("R", "random_forest", "grid_and_bootstrap_multi_fits.r"),
  file.path("R", "random_forest", "grid_up_foi_dataset.r"),
  file.path("R", "random_forest", "bootstrap_foi_dataset.r"),
  file.path("R", "utility_functions.r"))

my_pkgs <- c()

context::context_log_start()
ctx <- context::context_save(path = "context",
                             sources = my_resources,
                             packages = my_pkgs)


# ---------------------------------------- define parameters


no_fits <- 200

dependent_variable <- "FOI"

grid_size <- 5

pseudoAbs_value <- 0

all_wgt <- 1

pAbs_wgt <- 0.25


# ---------------------------------------- are you using the cluster? 


if (CLUSTER) {
  
  obj <- didehpc::queue_didehpc(ctx)
  
} else {
  
  context::context_load(ctx)
  
}


# ---------------------------------------- load data


foi_data <- read.csv(
  file.path("output", "foi", "All_FOI_estimates_linear_env_var.csv"),
  stringsAsFactors = FALSE) 


# ---------------------------------------- pre process the original foi dataset


foi_data[foi_data$type == "pseudoAbsence", dependent_variable] <- pseudoAbs_value

foi_data$new_weight <- all_wgt

foi_data[foi_data$type == "pseudoAbsence", "new_weight"] <- pAbs_wgt

names(foi_data)[names(foi_data) == dependent_variable] <- "o_j"


# ---------------------------------------- submit job


if (CLUSTER) {
  
  get_boot_samples <- queuer::qlapply(
    seq_len(no_fits),
    grid_and_boot_multi,
    obj,
    a = foi_data, 
    b = grid_size) 
  
} else {
  
  get_boot_samples <- lapply(
    seq_len(no_fits)[1:2],
    grid_and_boot_multi,
    a = foi_data, 
    b = grid_size)
  
}   
