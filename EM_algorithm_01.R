# Filters each 1km tile based on the original foi dataset  
# and resamples each tile to 20km resolution

options(didehpc.cluster = "fi--didemrchnb")

CLUSTER <- TRUE

my_resources <- c(
  file.path("R", "prepare_datasets", "filter_and_resample.R"),
  file.path("R", "prepare_datasets", "clean_and_average.R"),
  file.path("R", "prepare_datasets", "remove_NA_rows.R"),
  file.path("R", "prepare_datasets", "grid_up.R"),
  file.path("R", "prepare_datasets", "average_up.R"))

my_pkgs <- c("data.table", "dplyr")

context::context_log_start()
ctx <- context::context_save(path = "context",
                             packages = my_pkgs,
                             sources = my_resources)


# define parameters ----------------------------------------------------------- 


parameters <- list(
  resample_grid_size = 20,
  no_predictors = 26)   
  
group_fields <- c("cell", "latitude", "longitude")

join_fields <- c("data_id", "ID_0", "ID_1")

in_pt <- file.path("output", "env_variables", "tile_set_2", "gadm")

resample <- TRUE

foi_dts_nm <- "All_FOI_estimates_linear_env_var_area_salje.csv"

extra_predictors <- NULL


# define variables ------------------------------------------------------------


new_res <- (1 / 120) * parameters$resample_grid_size


# are you using the cluster? -------------------------------------------------- 


if (CLUSTER) {
  
  obj <- didehpc::queue_didehpc(ctx)

} else {
  
  context::context_load(ctx)

}


# load data ------------------------------------------------------------------- 


foi_data <- read.csv(file.path("output", "foi", foi_dts_nm),
                     stringsAsFactors = FALSE)

predictor_rank <- read.csv(file.path("output", 
                                     "variable_selection",
                                     "stepwise",
                                     "predictor_rank.csv"), 
                           stringsAsFactors = FALSE)


# pre processing -------------------------------------------------------------- 


fi <- list.files(in_pt, pattern = "^tile", full.names = TRUE)

my_predictors <- predictor_rank$name[1:parameters$no_predictors]
my_predictors <- setdiff(my_predictors, "log_pop_den") #`log_pop_den` is not in the original tile set
my_predictors <- c(my_predictors, extra_predictors)
  

# submit one test job --------------------------------------------------------- 


# t <- obj$enqueue(
#   filter_and_resample(
#     fi[1],
#     foi_dts = foi_data,
#     env_var_names = my_predictors,
#     grp_flds = group_fields,
#     jn_flds = join_fields,
#     grid_size = new_res,
#     resample = resample))


# submit all jobs ------------------------------------------------------------- 


if (CLUSTER) {

  pxl_job <- queuer::qlapply(
    fi,
    filter_and_resample,
    obj,
    foi_dts = foi_data,
    env_var_names = my_predictors,
    grp_flds = group_fields,
    jn_flds = join_fields,
    grid_size = new_res,
    resample = resample)

} else {

  pxl_job <- lapply(
    fi[5],
    filter_and_resample,
    foi_dts = foi_data,
    env_var_names = my_predictors,
    grp_flds = group_fields,
    jn_flds = join_fields,
    grid_size = new_res,
    resample = resample)

}
