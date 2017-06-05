rm(list = ls())

# devtools::install_github(c(
#    "dide-tools/context",
#    "richfitz/queuer",
#    "dide-tools/didewin"), force = TRUE, upgrade = FALSE)

my_resources <- c(file.path("R", "convert_df_to_list.R"),
                  file.path("R", "prepare_datasets", "wrapper_to_load_env_var.R"),
                  file.path("R", "prepare_datasets", "load_sets_of_environmental_variables.R"),
                  file.path("R", "prepare_datasets", "append_env_var_by_tile.R"),
                  file.path("R", "prepare_datasets", "wrapper_to_get_env_var_for_All_pixels.R"),
                  file.path("R", "prepare_datasets", "assign_NA_env_var_pixel.R"))

my_pkgs <- c("data.table", "raster", "rgdal")  

CLUSTER <- TRUE

if(CLUSTER) {
  
  # Load packages
  library(context)
  library(queuer)
  library(didewin)
  
  my_workdir <- "Q:/dengue_risk_mapping"
  
  didewin::didewin_config_global(cluster = "fi--didemrchnb", 
                                 workdir = my_workdir)
  
  root <- file.path(my_workdir, "context")
  
  ctx <- context::context_save(packages = my_pkgs,
                               sources = my_resources,
                               root = root)
  
  #config <- didewin::didewin_config(template = "16Core", parallel = FALSE)
  
  obj <- didewin::queue_didewin(ctx, sync = "R")
  
}else{
  
  # Load packages
  sapply(my_pkgs, library, character.only = TRUE)
  
  # Load functions 
  sapply(my_resources, source)
  
}

# load data 
tiles <- read.csv(file.path("data", "env_variables", "plus60minus60_tiles.csv"), 
                  header = TRUE, sep = ",", stringsAsFactors = FALSE)

landscan_pop <- raster("//fi--didef2/Census/Landscan2014/Population/lspop2014.flt")

country_adm0 <- readShapePoly(file.path("data", "shapefiles", "gadm28_levels.shp", "gadm28_adm0.shp"))
  

# ---------------------------------------- define variable objects and names 


alt_vars <- c("\"id\"", "\"latitude\"", "\"longitude\"", "\"altitude\"")
alt_var_names <- c("pixel_id", "latitude", "longitude", "altitude")
FTs_data <- c("DayTemp", "NightTemp", "EVI", "MIR", "RFE")
FTs_data_elements <- c("const_term",	"Re0", "Im0",	"Re1",	"Im1")
FTs_vars <- lapply(FTs_data, paste, FTs_data_elements, sep = "_")
land_cover_vars <- c(
  "lct1_2012001_0",	"lct1_2012001_1",	"lct1_2012001_2",	"lct1_2012001_3",	
  "lct1_2012001_4",	"lct1_2012001_5",	"lct1_2012001_6",	"lct1_2012001_7",	
  "lct1_2012001_8",	"lct1_2012001_9",	"lct1_2012001_10",	"lct1_2012001_11",
  "lct1_2012001_12",	"lct1_2012001_13",	"lct1_2012001_14",	"lct1_2012001_15",
  "lct1_2012001_16",	"lct1_2012001_254",	"lct1_2012001_255")

all_vars <- c(list(alt_vars), rep(list(FTs_data_elements), length(FTs_data)), list(land_cover_vars))
all_var_names <- c(list(alt_var_names), FTs_vars, list(land_cover_vars))

file_roots <- c("alt_global_raw_set", 
                "FTfreqs_2007-2014_64ppyear", 
                "MCD12Q1.A.051.lct1_2012_5km_set")


# ---------------------------------------- start


tiles$set_id <- tiles$tile.id
  
tiles_lst <- df_to_list(tiles, use_names = TRUE)

if (CLUSTER) {
  
  #error_ones <- which(write_out_tiles$status() == "ERROR")
  
  write_out_tiles <- queuer::qlapply(
    tiles_lst, 
    wrapper_to_get_env_var_for_pixels,
    obj,
    a = "//fi--didenas3/Dengue/Data/processed/fullres",
    b = FTs_data,
    c = file_roots,
    d = all_vars, 
    e = all_var_names,
    pop_raster = landscan_pop)
  
}else{
    
  # do not do this for the entire list on a local machine
  write_out_tiles <- lapply(
    tiles_lst[1], 
    wrapper_to_get_env_var_for_pixels,
    a = "//fi--didenas3/Dengue/Data/processed/fullres",
    b = FTs_data,
    c = file_roots,
    d = all_vars, 
    e = all_var_names,
    pop_raster = landscan_pop)
  
}  
  