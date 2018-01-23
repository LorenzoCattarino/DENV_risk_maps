library(h2o)

model_type <- "R0_3_boot_model"

model_in_path <- file.path("output",
                           "EM_algorithm",
                           model_type,
                           "optimized_model_objects")

RF_mod_name <- "RF_obj_sample"

i <- 1

cols <- c("DayTemp_Re1", "altitude")

RF_obj_nm <- paste0(RF_mod_name, "_", i, ".rds")

h2o.init()

RF_obj <- h2o.loadModel(file.path(model_in_path, RF_obj_nm))

dat <- readRDS(file.path("output", 
                         "EM_algorithm",
                         "env_variables_R0_3_fit",
                         "boot_samples",
                         paste0("env_vars_and_foi_20km_", i, ".rds")))
  
dat_h2o <- as.h2o(dat)

pdp <- h2o.partialPlot(RF_obj, dat_h2o, cols, plot = TRUE, plot_stddev = TRUE)

