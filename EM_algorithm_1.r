# Filters each 1km tile based on the original foi dataset  
# and resamples each tile to 20km resolution

options(didehpc.cluster = "fi--didemrchnb")

CLUSTER <- TRUE

my_resources <- c(
  file.path("R", "prepare_datasets", "filter_and_resample.r"),
  file.path("R", "prepare_datasets", "clean_and_resample.r"),
  file.path("R", "prepare_datasets", "remove_NA_rows.r"),
  file.path("R", "prepare_datasets", "grid_up.R"),
  file.path("R", "prepare_datasets", "average_up.r"))

my_pkgs <- c("data.table", "dplyr")

context::context_log_start()
ctx <- context::context_save(path = "context",
                             packages = my_pkgs,
                             sources = my_resources)


# ---------------------------------------- define parameters


in_pt <- file.path("data", "env_variables", "all_sets_gadm_codes")

group_fields <- c("data_id", "ADM_0", "ADM_1")

gr_size <- 20
  
new_res <- (1 / 120) * gr_size


# ---------------------------------------- are you using the cluster? 


if (CLUSTER) {
  
  obj <- didehpc::queue_didehpc(ctx)

} else {
  
  context::context_load(ctx)

}


# ---------------------------------------- load data


predictor_rank <- read.csv(
  file.path("output", 
            "variable_selection", 
            "metropolis_hastings", 
            "exp_1", 
            "variable_rank_final_fits_exp_1.csv"),
  stringsAsFactors = FALSE)

foi_data <- read.csv(
  file.path("output", "foi", "All_FOI_estimates_linear_env_var_area.csv"),
  stringsAsFactors = FALSE)


# ---------------------------------------- pre processing


names(foi_data)[names(foi_data) == "ID_0"] <- "ADM_0"
names(foi_data)[names(foi_data) == "ID_1"] <- "ADM_1"
names(foi_data)[names(foi_data) == "population"] <- "adm_pop"

my_predictors <- predictor_rank$variable[1:9]

my_predictors <- c(my_predictors, "RFE_const_term")

fi <- list.files(in_pt, 
                 pattern = "^tile",
                 full.names = TRUE)


# ---------------------------------------- submit one test job


# t <- obj$enqueue(
#   filter_and_resample(
#     fi[1],
#     foi_dts = foi_data,
#     env_var_names = my_predictors,
#     grp_flds = group_fields,
#     grid_size = new_res))


# ---------------------------------------- submit all jobs


if (CLUSTER) {

  pxl_job <- queuer::qlapply(
    fi,
    filter_and_resample,
    obj,
    foi_dts = foi_data,
    env_var_names = my_predictors,
    grp_flds = group_fields,
    grid_size = new_res)

} else {

  pxl_job <- lapply(
    fi[159],
    filter_and_resample,
    foi_dts = foi_data,
    env_var_names = my_predictors,
    grp_flds = group_fields,
    grid_size = new_res)

}
