# This assign a UNIQUE id to each point in the bootstrap sample
# e.g. If point 1 from original dataset is repeated twice, 
# the first instance in the bootstrap sample will have id = 1,
# the second instance will have id = 2.

source(file.path("R", "prepare_datasets", "functions_for_creating_bootstrap_samples.r"))
source(file.path("R", "utility_functions.r"))

boot_samples <- readRDS(
  file.path("output",
            "EM_algorithm",
            "bootstrap_samples.rds"))

if (names(boot_samples[[1]])[1] != "unique_id") {
  
  test <- lapply(seq_along(boot_samples), attach_unique_id, boot_samples)
  
  write_out_rds(test, 
                file.path("output",
                          "EM_algorithm"),
                "bootstrap_samples.rds")
  
}
