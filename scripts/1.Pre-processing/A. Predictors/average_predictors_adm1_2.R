
# binds all tiles together 

options(didehpc.cluster = "fi--didemrchnb")

my_resources <- c(
  file.path("R", "prepare_datasets", "average_up.R"),
  file.path("R", "utility_functions.R"))

my_pkgs <- c("dplyr", "data.table")

context::context_log_start()
ctx <- context::context_save(path = "context",
                             sources = my_resources,
                             packages = my_pkgs)

context::context_load(ctx)
context::parallel_cluster_start(8, ctx)


# define parameters -----------------------------------------------------------  


group_fields <- c("ID_0", "ID_1")

in_pt <- file.path("output", "env_variables", "tile_set_adm1")

altitude_var <- "altitude"
fourier_transform_elem <- c("const_term",	"Re0",	"Im0",	"Re1",	"Im1")
FTs_dt <- c("DayTemp", "EVI", "MIR", "NightTemp", "RFE")

out_fl_nm <- "all_adm1_env_var.csv"

out_pt <- file.path("output", "env_variables")


# pre processing -------------------------------------------------------------- 


all_combs_df <- expand.grid(FTs_dt, fourier_transform_elem)

all_combs_names <- apply(all_combs_df, 1, function(x) paste(x[1], x[2], sep = "_"))

my_predictors <- c(altitude_var, all_combs_names)

my_predictors <- c(my_predictors, "travel_time", "TSI", "aedes_gen")

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

all_adm1_covariates_dupl <- do.call("rbind", all_tiles)

ret <- average_up(all_adm1_covariates_dupl, group_fields, my_predictors)

ret_2 <- ret[!duplicated(ret[, c("ID_0", "ID_1")]), ]


# save ------------------------------------------------------------------------


write_out_csv(ret_2, out_pt, out_fl_nm)

