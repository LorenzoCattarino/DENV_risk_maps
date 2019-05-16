rm(list=ls())

# load R packages
library(dplyr)

# load functions 
source(file.path("R", "prepare_datasets", "combine_RUS_CAN_adm2_env_out.R"))

# create vectors of variable names 
altitude_var <- "altitude"
fourier_transform_elem <- c("const_term",	"Re0", "Im0",	"weight0",	"Re1",	"Im1",	"weight1", "Re2",	
                            "Im2",	"weight2",	"Re3",	"Im3",	"weight3",	"Re4",	"Im4",	"weight4")
FTs_dt <- c("DayTemp", "EVI", "MIR", "NightTemp", "RFE")
LandCover_var <- c("lct1_2012001_0",	"lct1_2012001_1",	"lct1_2012001_2",	"lct1_2012001_3",	
                   "lct1_2012001_4",	"lct1_2012001_5",	"lct1_2012001_6",	"lct1_2012001_7",	
                   "lct1_2012001_8",	"lct1_2012001_9",	"lct1_2012001_10",	"lct1_2012001_11",
                   "lct1_2012001_12",	"lct1_2012001_13",	"lct1_2012001_14",	"lct1_2012001_15",
                   "lct1_2012001_16",	"lct1_2012001_254",	"lct1_2012001_255")
country_code_vec <- c("RUS", "CAN")

# Run the appending

sapply(country_code_vec, 
       append.and.aggregate.CAN_RUS_adm2, 
       altitude_variables = altitude_var, 
       fourier_transform_elements = fourier_transform_elem, 
       FTs_data = FTs_dt, 
       LandCover_variables = LandCover_var, 
       destination_folder = "data", 
       origin_admin_level = 2,
       aggregation_admin_level = 1)

# "//fi--didenas3/Dengue/Data"
# "data"