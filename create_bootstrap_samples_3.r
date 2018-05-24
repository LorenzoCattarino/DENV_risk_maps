# This assign a UNIQUE id to each point in the bootstrap sample
# e.g. If point 1 from original dataset is repeated twice, 
# the first instance in the bootstrap sample will have id = 1,
# the second instance will have id = 2.

source(file.path("R", "prepare_datasets", "functions_for_creating_bootstrap_samples.R"))
source(file.path("R", "utility_functions.R"))

parameters <- list(
  grid_size = 10,
  all_wgt = 1,
  wgt_limits = c(1, 500),
  no_samples = 200)   

my_dir <- paste0("grid_size_", parameters$grid_size)

out_pth <- file.path("output", 
                     "EM_algorithm", 
                     "bootstrap_models", 
                     my_dir)
  
boot_samples <- readRDS(file.path("output", 
                                  "EM_algorithm", 
                                  "bootstrap_models", 
                                  my_dir, 
                                  "bootstrap_samples.rds"))

if (names(boot_samples[[1]])[1] != "unique_id") {
  
  test <- lapply(seq_along(boot_samples), attach_unique_id, boot_samples)
  
  write_out_rds(test, out_pth, "bootstrap_samples.rds")
  
}
