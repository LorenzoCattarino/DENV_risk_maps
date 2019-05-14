# Extract the values of the predictor for each 1km pixel of the world 
# and store them in separate `tiles`  

options(didehpc.cluster = "fi--didemrchnb")

CLUSTER <- TRUE

my_resources <- c(
  file.path("R", "prepare_datasets", "extract_predictors.R"),
  file.path("R", "utility_functions.R"))

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

in_path <- file.path("H:", "Data", "processed", "fullres")

out_path <- file.path("output", "env_variables", "tile_set_2")


# are you using the cluster? --------------------------------------------------
  
  
if(CLUSTER) {
  
  share <- didehpc::path_mapping("dengue", "H:", "//fi--didenas3/Dengue", "H:")
  config <- didehpc::didehpc_config(shares = share)
  obj <- didehpc::queue_didehpc(ctx, config = config)
  
} else {
  
  context::context_load(ctx)
  
}


# load data -------------------------------------------------------------------


tiles <- read.csv(file.path("data", 
                            "env_variables", 
                            "plus60minus60_tiles.csv"), 
                  stringsAsFactors = FALSE)

landscan_pop <- raster(file.path("data", 
                                 "Landscan_2015", 
                                 "lspop2015.flt")) 
# used to be "//fi--didef2/Census/Landscan2014/Population/lspop2014.flt"

accessibility <- raster(file.path("data", 
                                  "sam's_predictors", 
                                  "accessibility_50k_5km.tif"))

temp_suitability <- raster(file.path("data", 
                                     "sam's_predictors", 
                                     "Pv_temperature_suitability.tif")) 

aedes_gen <- raster(file.path("data", 
                              "aedes_generations", 
                              "eggsgen_ck_2005-2010_Global_arg.gri"))


# pre processing -------------------------------------------------------------- 


FTs_vars <- lapply(FTs_data, paste, FTs_data_elements, sep = "_")

all_vars <- c(list(alt_vars), rep(list(FTs_data_elements), length(FTs_data)), list(LandCover_vars))

all_var_names <- c(list(alt_var_names), FTs_vars, list(LandCover_vars))

tiles$set_id <- tiles$tile.id
  
tiles_lst <- df_to_list(tiles, use_names = TRUE)

NAvalue(accessibility) = -9999
NAvalue(temp_suitability) = -9999


# submit one job --------------------------------------------------------------


# t <- obj$enqueue(
#   wrapper_to_get_env_var_for_pixels(
#     tiles_lst[[72]],
#     in_path = in_path,
#     out_path = out_path,
#     FTs_data = FTs_data,
#     txt_file_roots = file_roots,
#     all_vars = all_vars,
#     all_var_names = all_var_names,
#     raster_1 = landscan_pop,
#     raster_2 = accessibility,
#     raster_3 = temp_suitability,
#     raster_4 = aedes_gen))


# submit all jobs -------------------------------------------------------------


if (CLUSTER) {

  write_out_tiles <- queuer::qlapply(
    tiles_lst,
    wrapper_to_get_env_var_for_pixels,
    obj,
    in_path = in_path,
    out_path = out_path,
    FTs_data = FTs_data,
    txt_file_roots = file_roots,
    all_vars = all_vars,
    all_var_names = all_var_names,
    raster_1 = landscan_pop,
    raster_2 = accessibility,
    raster_3 = temp_suitability,
    raster_4 = aedes_gen)

} else {

  write_out_tiles <- lapply(
    tiles_lst[1],
    wrapper_to_get_env_var_for_pixels,
    in_path = in_path,
    out_path = out_path,
    FTs_data = FTs_data,
    txt_file_roots = file_roots,
    all_vars = all_vars,
    all_var_names = all_var_names,
    raster_1 = landscan_pop,
    raster_2 = accessibility,
    raster_3 = temp_suitability,
    raster_4 = aedes_gen)

}
