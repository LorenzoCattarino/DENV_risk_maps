########
########
########


# ==================================== Code needs testing!!!!!


########
########
########


# Extract the values of the predictor for each 1x1 km pixel of the world 
# and store them in separate `tiles`  

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


alt_vars <- c("\"id\"", "\"latitude\"", "\"longitude\"", "\"altitude\"")

alt_var_names <- c("pixel_id", "latitude", "longitude", "altitude")

FTs_data <- c("DayTemp", "NightTemp", "EVI", "MIR", "RFE")

FTs_data_elements <- c("const_term",	"Re0", "Im0",	"Re1",	"Im1")

LandCover_vars <- paste("lct1_2012001", c(seq(0, 16, 1), 254, 255), sep = "_")

file_roots <- c("alt_global_raw_set", 
                "FTfreqs_2007-2014_64ppyear", 
                "MCD12Q1.A.051.lct1_2012_5km_set")

in_path <- "fullres"


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

landscan_pop <- raster(file.path("data", "Landscan_2015", "lspop2015.flt")) 
# used to be "//fi--didef2/Census/Landscan2014/Population/lspop2014.flt"

aedes_gen <- raster(file.path("data", 
                              "aedes_generations", 
                              "eggsgen_ck_2005-2010_Global_arg.gri"))


# pre processing -------------------------------------------------------------- 


FTs_vars <- lapply(FTs_data, paste, FTs_data_elements, sep = "_")

all_vars <- c(list(alt_vars), rep(list(FTs_data_elements), length(FTs_data)), list(LandCover_vars))

all_var_names <- c(list(alt_var_names), FTs_vars, list(LandCover_vars))

tiles$set_id <- tiles$tile.id
  
tiles_lst <- df_to_list(tiles, use_names = TRUE)


# submit one job --------------------------------------------------------------


t <- obj$enqueue(
  wrapper_to_get_env_var_for_pixels(
    my_path = in_path,
    FTs_data = FTs_data,
    txt_file_roots = file_roots,
    all_vars = all_vars, 
    all_var_names = all_var_names,
    pop_raster = landscan_pop,
    aedes_raster = aedes_raster))


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
    pop_raster = landscan_pop,
    aedes_raster = aedes_raster)
  
}else{
    
  write_out_tiles <- lapply(
    tiles_lst[1], 
    wrapper_to_get_env_var_for_pixels,
    my_path = in_path,
    FTs_data = FTs_data,
    txt_file_roots = file_roots,
    all_vars = all_vars, 
    all_var_names = all_var_names,
    pop_raster = landscan_pop,
    aedes_raster = aedes_raster)
  
}  
  