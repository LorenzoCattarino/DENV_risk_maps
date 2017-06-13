options(didehpc.cluster = "fi--didemrchnb")

CLUSTER <- TRUE

my_resources <- c(
  file.path("R", "random_forest", "grid_and_boot.r"),
  file.path("R", "random_forest", "grid_up_foi_dataset.r"),
  file.path("R", "random_forest", "bootstrap_foi_dataset.r"),
  file.path("R", "utility_functions.r"))

my_pkgs <- c()

context::context_log_start()
ctx <- context::context_save(path = "context",
                             sources = my_resources,
                             packages = my_pkgs)


# ---------------------------------------- are you using the cluster? 


if (CLUSTER) {
  
  config <- didehpc::didehpc_config(template = "24Core")
  obj <- didehpc::queue_didehpc(ctx, config = config)
  
} else {
  
  context::context_load(ctx)
  context::parallel_cluster_start(8, ctx)
  
}


# ---------------------------------------- define parameters


no_fits <- 200


# ---------------------------------------- load data


foi_data <- read.csv(
  file.path("output", "foi", "All_FOI_estimates_linear_env_var.csv"),
  stringsAsFactors = FALSE) 


# ---------------------------------------- submit job


if (CLUSTER) {
  
  get_boot_samples <- queuer::qlapply(
    seq_len(no_fits),
    grid_and_boot()) 

} else {

  get_boot_samples <- lapply(
    seq_len(no_fits),
    
  )
  
}   
    
    
    