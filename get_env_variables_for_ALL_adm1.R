# load packages
library(maptools)

# load functions
source(file.path("R", "prepare_datasets", "get_env_variables.R"))
source(file.path("R", "utility_functions.r"))

# load data 
shp_fl_adm1 <- readShapePoly(file.path("data", "shapefiles", "gadm28_levels.shp", "gadm28_adm1.shp"))


# ---------------------------------------- define variable objects and names 


population_var <- "pop"
altitude_var <- "altitude"
fourier_transform_elem <- c("const_term",	"Re0",	"Im0",	"Re1",	"Im1")
FTs_dt <- c("DayTemp", "EVI", "MIR", "NightTemp", "RFE")
LandCover_var <- paste("lct1_2012001", c(seq(0, 16, 1), 254, 255), sep = "_")

# Get number of variables to extract
number_of_variables <- length(c(altitude_var, LandCover_var)) + (length(c(population_var, fourier_transform_elem)) * length(FTs_dt))

all_var_names <- c(altitude_var, 
                   apply(expand.grid(c(population_var, fourier_transform_elem), FTs_dt), 1, function(x) paste(x[2],x[1], sep="_")), 
                   LandCover_var)

base_info <- c("OBJECTID", "ID_0", "ISO", "country", "ID_1", "name1", "latitude", "longitude")


# ---------------------------------------- start


# get polygon coordinates 
long_lat <- coordinates(shp_fl_adm1)

all_adm1 <- cbind(shp_fl_adm1@data[, c("OBJECTID", "ID_0", "ISO", "NAME_0", "ID_1", "NAME_1")], long_lat[, c(2, 1)]) 
names(all_adm1) <- base_info

all_adm1$ISO <- as.character(all_adm1$ISO)
all_adm1$country <- as.character(all_adm1$country)
all_adm1$name1 <- as.character(all_adm1$name1)

all_adm1_ls <- df_to_list(all_adm1, use_names = TRUE)

# extract env variables 
env_var_values <- sapply(all_adm1_ls,
                         get_env_variables, 
                         no_vars = number_of_variables, 
                         pop_vars = population_var,
                         alt_vars = altitude_var, 
                         FT_elements = fourier_transform_elem, 
                         FT_data = FTs_dt, 
                         LC_vars = LandCover_var, 
                         admin_level = 1,
                         my_path = file.path("data", "env_variables"))

adm1_env_var <- cbind(all_adm1, t(env_var_values))

colnames(adm1_env_var) <- c(names(all_adm1), all_var_names)


# ---------------------------------------- remove redundant population columns (not very elegant)


pop_col_names <- grep("pop", colnames(adm1_env_var), value = TRUE)

adm1_env_var <- adm1_env_var[setdiff(names(adm1_env_var), pop_col_names[(length(pop_col_names):2)])]

pop_col_names <- grep("pop", colnames(adm1_env_var), value = FALSE)

colnames(adm1_env_var)[pop_col_names] <- "population"
  

# ---------------------------------------- get rows with NA values 


row.has.na <- apply(adm1_env_var, 1, function(x){any(is.na(x))})

NA_adm1_anv_var <- adm1_env_var[row.has.na, base_info[1:5]]


# ---------------------------------------- write out 


write.table(adm1_env_var[order(adm1_env_var$country, adm1_env_var$ID_1), ], 
            file.path("output", 
                      "env_variables", 
                      "All_adm1_env_var.csv"), 
            row.names = FALSE, sep = ",")

write.table(NA_adm1_anv_var[order(NA_adm1_anv_var$country, NA_adm1_anv_var$ID_1), ], 
            file.path("output", 
                      "env_variables", 
                      "All_adm1_env_var_NA.csv"), 
            row.names = FALSE, sep = ",")
