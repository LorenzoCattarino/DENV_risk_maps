
# resample all the 1 km pixels, in each tile, to squares with a coarser resolution
# average the value of the pixel predictors within each square

options(didehpc.cluster = "fi--didemrchnb")

CLUSTER <- FALSE

my_resources <- c(
  file.path("R", "prepare_datasets", "load_clean_and_average.R"),
  file.path("R", "prepare_datasets", "clean_and_average.R"),
  file.path("R", "prepare_datasets", "grid_up.R"),
  file.path("R", "prepare_datasets", "remove_NA_rows.R"),
  file.path("R", "prepare_datasets", "average_up.R"))

my_pkgs <- c("data.table", "dplyr")

context::context_log_start()
ctx <- context::context_save(path = "context",
                             packages = my_pkgs,
                             sources = my_resources)


# define parameters ----------------------------------------------------------- 


parameters <- list(
  grid_size = 1 / 120,       # decimal degrees
  resample_grid_size = 20)   # kms  

group_fields <- c("cell", "latitude", "longitude")

in_pt <- file.path("output", "env_variables", "tile_set_2", "gadm")

altitude_var <- "altitude"
fourier_transform_elem <- c("const_term",	"Re0",	"Im0",	"Re1",	"Im1")
FTs_dt <- c("DayTemp", "EVI", "MIR", "NightTemp", "RFE")

resample <- TRUE


# define variables ------------------------------------------------------------


re_gr_size <- parameters$resample_grid_size 

new_res <- parameters$grid_size * re_gr_size

out_pt <- file.path("output",
                    "env_variables",
                    paste0("tile_set_2_", re_gr_size, "km"))


# are you using the cluster? --------------------------------------------------  


if (CLUSTER) {
  
  config <- didehpc::didehpc_config(template = "20Core")
  obj <- didehpc::queue_didehpc(ctx, config = config)
  
} else {
  
  context::context_load(ctx)
  
}


# pre processing -------------------------------------------------------------- 


all_combs_df <- expand.grid(FTs_dt, fourier_transform_elem)

all_combs_names <- apply(all_combs_df, 1, function(x) paste(x[1], x[2], sep = "_"))

var_names <- c(altitude_var, all_combs_names)
                   
var_names <- c(var_names, "travel_time", "TSI", "aedes_gen")

fi <- list.files(in_pt, 
                 pattern = "^tile",
                 full.names = TRUE)


# submit one job -------------------------------------------------------------- 


# t <- obj$enqueue(
#   load_clean_and_average(fi[186],
#   grp_flds = group_fields,
#   grid_size = new_res,
#   env_var_names = var_names,
#   out_path = out_pt,
#   resample = resample))


# submit all jobs ------------------------------------------------------------- 


if (CLUSTER) {

  resample_tiles <- queuer::qlapply(
    fi,
    load_clean_and_average,
    obj,
    grp_flds = group_fields,
    grid_size = new_res,
    env_var_names = var_names,
    out_path = out_pt,
    resample = resample)

}else{

  resample_tiles <- lapply(
    fi[45],
    load_clean_and_average,
    grp_flds = group_fields,
    grid_size = new_res,
    env_var_names = var_names,
    out_path = out_pt,
    resample = resample)

}
