
# average up the 70 Salje points within the 7 bangladesh admin units
# extract predictor values for each of the 7 points

library(rgdal)
library(dplyr)

source(file.path("R", "prepare_datasets", "get_env_variables.R"))
source(file.path("R", "utility_functions.R"))


# define parameters -----------------------------------------------------------


pop_var <- "pop"
alt_var <- "altitude"
FT_elem <- c("const_term",	"Re0",	"Im0",	"Re1",	"Im1")
FT_var <- c("RFE", "DayTemp", "NightTemp", "EVI", "MIR")
LC_var <- paste("lct1_2012001", c(seq(0, 16, 1), 254, 255), sep = "_")

base_info <- c("data_id", "type", "longitude", "latitude", "ISO", "ID_0", "ID_1", "FOI")

dts_out_pt <- file.path("output", "seroprevalence", "salje")
  
dts_out_nm <- "observations_adm1.csv" 

prj_crs <- CRS("+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")


# load data -------------------------------------------------------------------


foi_dataset <- read.csv(file.path("output", 
                                  "foi", 
                                  "All_FOI_estimates_linear_env_var_area.csv"),
                        stringsAsFactors = FALSE) 

salje_data <- read.csv(file.path("output", 
                                 "seroprevalence",
                                 "salje",
                                 "observations_20km.csv"),
                       stringsAsFactors = FALSE)

shp <- readOGR(file.path("data", "shapefiles", "gadm28_levels.shp"), "gadm28_adm1")


# pre processing --------------------------------------------------------------


salje_data$data_id <- seq_len(nrow(salje_data))
salje_data$type <- "serology"

salje_data <- rename(salje_data, longitude = lon)
salje_data <- rename(salje_data, latitude = lat)


# keep ID_1 unique salje points -----------------------------------------------


ave_foi <- group_by(salje_data, ID_1) %>%
  summarise(FOI = mean(foi)) %>% 
  left_join(distinct(salje_data, ID_1, .keep_all = T))

ave_foi <- ave_foi[, base_info]


# extract env variables -------------------------------------------------------


ave_foi_ls <- df_to_list(ave_foi, use_names = TRUE)

number_of_variables <- length(c(alt_var, LC_var)) + (length(c(pop_var, FT_elem)) * length(FT_var))

extracted_var_values <- sapply(
  ave_foi_ls,
  get_env_variables,
  no_vars = number_of_variables,
  pop_vars = pop_var,
  alt_vars = alt_var,
  FT_elements = FT_elem,
  FT_data = FT_var,
  LC_vars = LC_var,
  admin_level = 1,
  my_path = file.path("output", "env_variables"))

ave_foi_2 <- cbind(ave_foi, t(extracted_var_values))

colnames(ave_foi_2) <- c(names(ave_foi), alt_var,
                            apply(expand.grid(c(pop_var, FT_elem), FT_var),
                                  1,
                                  function(x) {paste(x[2],x[1], sep="_")}), LC_var)


# Remove redundant population columns -----------------------------------------


pop_col_names <- grep("pop", colnames(ave_foi_2), value = TRUE)

ave_foi_2 <- ave_foi_2[setdiff(names(ave_foi_2), pop_col_names[(length(pop_col_names):2)])]

pop_col_names <- grep("pop", colnames(ave_foi_2), value = FALSE)

colnames(ave_foi_2)[pop_col_names] <- "population"


# find adm1 area and pop density ----------------------------------------------


shp_pr <- spTransform(shp, prj_crs)

all_areas <- sapply(shp_pr@polygons, function(x) x@area)

# from sq m to sq km
shp_pr@data$Shape_Area <- all_areas/1000000 

shp_pr@data$ID_0 <- as.numeric(as.character(shp_pr@data$ID_0))
shp_pr@data$ID_1 <- as.numeric(as.character(shp_pr@data$ID_1))

shp_pr@data <- shp_pr@data[!duplicated(shp_pr@data[, c("ID_0", "ID_1")]), ]

ave_foi_2 <- left_join(ave_foi_2, shp_pr@data[, c("ID_0", "ID_1", "Shape_Area")], by = c("ID_0", "ID_1"))

ave_foi_2$pop_den <- ave_foi_2$population / ave_foi_2$Shape_Area  


# save dataset ----------------------------------------------------------------


write_out_csv(ave_foi_2, dts_out_pt, dts_out_nm)
