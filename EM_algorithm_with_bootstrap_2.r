# Binds different bootstrap samples together in a list and save the list 

options(didehpc.cluster = "fi--didemrchnb")

CLUSTER <- TRUE

my_resources <- c(
  file.path("R", "random_forest", "grid_and_bootstrap.r"),
  file.path("R", "random_forest", "grid_up_foi_dataset.r"),
  file.path("R", "random_forest", "bootstrap_foi_dataset.r"),
  file.path("R", "utility_functions.r"))

my_pkgs <- c()

context::context_log_start()
ctx <- context::context_save(path = "context",
                             sources = my_resources,
                             packages = my_pkgs)


# ---------------------------------------- define parameters


out_fl_nm <- "bootstrap_samples.rds"

out_pt <- file.path("output", "EM_algorithm")


# ---------------------------------------- rebuild the queue obj


if (CLUSTER) {
  
  obj <- didehpc::queue_didehpc(ctx)
  
} else {
  
  context::context_load(ctx)
  
}

task_b_name <- "chiroptophobic_auklet"

get_boot_samples_t <- obj$task_bundle_get(task_b_name)


# ---------------------------------------- get the results 


boot_samples <- get_boot_samples_t$results()


# ---------------------------------------- save

write_out_rds(boot_samples, out_pt, out_fl_nm)
