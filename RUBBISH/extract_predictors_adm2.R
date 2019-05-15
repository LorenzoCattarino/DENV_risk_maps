# load packages
library(rgdal)
library(data.table)

# load functions 
source(file.path("R", "utility_functions.R"))
source(file.path("R", "prepare_datasets", "functions_for_extracting_predictor_values.R"))
source(file.path("R", "prepare_datasets", "append_env_var_by_country.R"))

# load data 
adm2_countries <- read.csv(file.path("output", 
                                     "datasets", 
                                     "all_adm_2_countries.csv"),
                           header = TRUE, 
                           stringsAsFactors = FALSE)

shp_fl_adm2 <- readOGR(file.path("data", "shapefiles", "gadm28_levels.shp"), 
                       "gadm28_adm2",
                       stringsAsFactors = FALSE)


# define parameters -----------------------------------------------------------


in_path <- file.path("output",
                     "env_variables",
                     "adm2")

target_info <- c("OBJECTID", 
                 "ID_0", 
                 "ISO", 
                 "NAME_0", 
                 "ID_1", 
                 "NAME_1", 
                 "ID_2", 
                 "NAME_2")

base_info <- c("OBJECTID", 
               "adm0", 
               "ISO", 
               "country", 
               "adm1", 
               "name1", 
               "adm2", 
               "name2", 
               "latitude", 
               "longitude")

alt_vars <- c("pop", "altitude")

alt_var_names <- c("population", "altitude")

FTs_data <- c("DayTemp", "NightTemp", "EVI", "MIR", "RFE")

FTs_data_elements <- c("const_term",	"Re0", "Im0",	"Re1",	"Im1")

above_60_countries <- c("FRO", "ISL", "SJM") 

file_roots <- c("alt_global_", 
                "FTfreqs_2007-2014_64ppyear", 
                "MCD12Q1.A.051.lct1_2012_5km_")

land_cover_vars <- paste("lct1_2012001", c(seq(0, 16, 1), 254, 255), sep = "_")


# create some lists of names  -------------------------------------------------


all_vars <- c(list(base_info), 
              list(alt_vars), 
              rep(list(FTs_data_elements), length(FTs_data)), list(land_cover_vars))

FTs_vars <- lapply(FTs_data, paste, FTs_data_elements, sep = "_")

all_var_names <- c(list(base_info), 
                   list(alt_var_names), 
                   FTs_vars, 
                   list(land_cover_vars))


# start -----------------------------------------------------------------------


long_lat <- coordinates(shp_fl_adm2)

all_adm2 <- cbind(shp_fl_adm2@data[, target_info], long_lat[, c(2, 1)]) 

names(all_adm2) <- base_info

all_adm2$adm0 <- as.numeric(all_adm2$adm0)
all_adm2$adm1 <- as.numeric(all_adm2$adm1)
all_adm2$adm2 <- as.numeric(all_adm2$adm2)


# remove countries above 60 degrees of latitude -------------------------------


adm2_countries <- adm2_countries[!adm2_countries$country_code %in% above_60_countries, ]


# -----------------------------------------------------------------------------


# create ids
adm2_countries$set_id <- paste("adm2", adm2_countries$country_code, sep = "_") 

# convert country df to list 
adm2_countries_ls <- df_to_list(adm2_countries, use_names = TRUE)

# load all env variables for each country
all_env_var <- lapply(adm2_countries_ls, 
                      wrapper_to_load_env_var, 
                      my_path = in_path, 
                      FTs_data = FTs_data,
                      txt_file_roots = file_roots)

all_starters <- lapply(adm2_countries_ls, function(x){
  subset(all_adm2, ISO == x$country_code)})
  
all_env_var_2 <- lapply(seq_along(all_starters), function(i){
  c(all_starters[i], all_env_var[[i]])})  

all_env_var_3 <- lapply(all_env_var_2, 
                        append_env_var_by_country, 
                        a = all_vars, 
                        b = all_var_names,
                        j_flds = c("adm1", "adm2"))

adm2_env_var <- do.call("rbind", all_env_var_3) 
      

# ---------------------------------------- get rows with NA values 


row.has.na <- apply(adm2_env_var, 1, function(x){any(is.na(x))})

NA_adm2_env_var <- adm2_env_var[row.has.na, base_info[1:8]]


# ---------------------------------------- remove rows with NA values 


adm2_env_var <- adm2_env_var[!row.has.na, ]


# ---------------------------------------- write out 


names(adm2_env_var)[names(adm2_env_var) == "adm0"] <- "ID_0"
names(adm2_env_var)[names(adm2_env_var) == "adm1"] <- "ID_1"
names(adm2_env_var)[names(adm2_env_var) == "adm2"] <- "ID_2"

names(NA_adm2_env_var)[names(NA_adm2_env_var) == "adm0"] <- "ID_0"
names(NA_adm2_env_var)[names(NA_adm2_env_var) == "adm1"] <- "ID_1"
names(NA_adm2_env_var)[names(NA_adm2_env_var) == "adm2"] <- "ID_2"

write.table(adm2_env_var[order(adm2_env_var$country, adm2_env_var$name1, adm2_env_var$name2),], 
            file.path("output", 
                      "env_variables", 
                      "All_adm2_env_var.csv"), 
            row.names = FALSE, sep = ",")

write.table(NA_adm2_env_var[order(NA_adm2_env_var$country, NA_adm2_env_var$name1, NA_adm2_env_var$name2),], 
            file.path("output", 
                      "env_variables", 
                      "All_adm2_env_var_NA.csv"), 
            row.names = FALSE, sep = ",")
