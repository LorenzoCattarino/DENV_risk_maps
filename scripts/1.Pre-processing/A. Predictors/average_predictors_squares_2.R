
# binds all tiles together 

options(didehpc.cluster = "fi--didemrchnb")

my_resources <- c(
  file.path("R", "utility_functions.R"))

my_pkgs <- c("dplyr", "data.table")

context::context_log_start()
ctx <- context::context_save(path = "context",
                             sources = my_resources,
                             packages = my_pkgs)

context::context_load(ctx)
context::parallel_cluster_start(8, ctx)


# define parameters -----------------------------------------------------------  


in_pt <- file.path("output", 
                   "env_variables",
                   "tile_set_20km",
                   "gadm")

out_fl_nm <- "all_squares_env_var_0_1667_deg.rds"

out_pt <- file.path("output", "env_variables")


# load data -------------------------------------------------------------------


age_struct_data <- read.csv(file.path("output",
                                    "datasets",
                                    "country_age_structure.csv"),
                          stringsAsFactors = FALSE)


# pre processing -------------------------------------------------------------- 


fi <- list.files(in_pt, pattern = "^tile", full.names = TRUE)


# combine all covariate tiles ------------------------------------------------- 


all_tiles <- loop(fi, 
                  fread,
                  header = TRUE,
                  sep = ",",
                  na.strings = c("NA", "-1.#IND", "Peipsi", "Moskva", "IJsselmeer", "Zeeuwse meren"),
                  fill = TRUE, 
                  data.table = FALSE,
                  parallel = TRUE)

context::parallel_cluster_stop()

all_sqr_covariates <- do.call("rbind", all_tiles)

all_sqr_covariates$cell <- seq_len(nrow(all_sqr_covariates))

all_sqr_covariates$log_pop_den <- log(1 + all_sqr_covariates$pop_den) 

all_sqr_covariates <- inner_join(all_sqr_covariates, 
                                 age_struct_data[, c("ID_0", "birth_rate", "mean_age", "sd_age")])

write_out_rds(all_sqr_covariates, out_pt, out_fl_nm)
