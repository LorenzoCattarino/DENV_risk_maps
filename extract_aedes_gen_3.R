options(didehpc.cluster = "fi--didemrchnb")

CLUSTER <- FALSE

my_resources <- c(
  file.path("R", "utility_functions.r"),
  file.path("R", "prepare_datasets", "functions_for_extracting_predictor_values.R"))

my_pkgs <- c("data.table", "raster", "rgdal")  

context::context_log_start()
ctx <- context::context_save(path = "context",
                             sources = my_resources,
                             packages = my_pkgs)


# define parameters ----------------------------------------------------------- 


in_pth <- file.path("output", "env_variables", "all_sets")

out_pth <- file.path("output", "env_variables", "all_sets_aedes")


# are you using the cluster? --------------------------------------------------


if(CLUSTER) {
  
  share <- didehpc::path_mapping("dengue", "H:", "//fi--didenas3", "H:")
  config <- didehpc::didehpc_config(shares = share)
  obj <- didehpc::queue_didehpc(ctx, config = config)
  
}else{
  
  context::context_load(ctx)
  
}

# load data -------------------------------------------------------------------


tiles <- read.csv(
  file.path("data", 
            "env_variables", 
            "plus60minus60_tiles.csv"), 
  header = TRUE,
  stringsAsFactors = FALSE)

aedes_gen <- raster(file.path("data", 
                              "aedes_generations", 
                              "eggsgen_ck_2005-2010_Global_arg.gri"))

# pre processing --------------------------------------------------------------


fi <- list.files(in_pth, 
                 pattern = "^tile",
                 full.names = TRUE)


# submit one job --------------------------------------------------------------


t <- obj$enqueue(
  append_raster_value_to_tile(fi[1],
                              my_raster = aedes_gen, 
                              out_path = out_pth))


# submit all jobs -------------------------------------------------------------


if (CLUSTER) {
  
  write_out_tiles <- queuer::qlapply(
    tiles_lst, 
    wrapper_to_get_env_var_for_pixels,
    obj,
    my_path = in_path,
    FTs_data = FTs_data,
    txt_file_roots = file_roots,
    all_vars = all_vars, 
    all_var_names = all_var_names,
    pop_raster = landscan_pop)
  
}else{
  
  write_out_tiles <- lapply(
    tiles_lst[1], 
    wrapper_to_get_env_var_for_pixels,
    my_path = in_path,
    FTs_data = FTs_data,
    txt_file_roots = file_roots,
    all_vars = all_vars, 
    all_var_names = all_var_names,
    pop_raster = landscan_pop)
  
}  
