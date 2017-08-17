# Extracts age structure information from source csv file
# (SOURCE: World Population Prospects - Population Division - United Nations)

# load packages
library(maptools)


# ---------------------------------------- define parameters 


col_classes <- c("integer", rep("character", 3), rep("integer", 2), rep("numeric", 22))

out_pt <- file.path("output", "datasets")

out_nm <- "country_age_structure.csv"


# ---------------------------------------- load data


age_struct_data <- read.csv(file.path("data",
                                      "population",
                                      "WPP2015_POP_F07_1_POPULATION_BY_AGE_BOTH_SEXES.csv"), 
                            colClasses = col_classes, na.strings = "…")

numerical_codes <- read.csv(file.path("data", "country_codes.csv")) 

country_border_shp_fl <- readShapePoly(
  file.path("data", 
            "shapefiles", 
            "gadm28_levels.shp", 
            "gadm28_adm0.shp"))


# ---------------------------------------- pre processing
  

# identify columns with age bands 
numeric_columns <- grep("band", names(age_struct_data), value = TRUE)

# scale population values back to thousands
age_struct_data[, numeric_columns] <- age_struct_data[, numeric_columns] * 1000 

# remove +100 age band from world dataset
age_struct_data <- age_struct_data[, setdiff(names(age_struct_data), "band_100_plus")]

numeric_columns <- numeric_columns[!numeric_columns == "band_100_plus"]


# ---------------------------------------- attach ADM_0 GADM country code  


# extract gadm ADM0 and ISO codes 
all_ID_0_ISOs <- country_border_shp_fl@data[, c("ISO", "ID_0", "NAME_ENGLI")]

# attach them to the numerical code 
numerical_codes_2 <- merge(numerical_codes, all_ID_0_ISOs, 
                           by.x = "ISO_alpha", 
                           by.y = "ISO",
                           all.y = TRUE) 

# all.y = T becasue I want to know for which countries there will be no numerical code (and therefore age str data) 

NA_ISO_numeric_logic <- is.na(numerical_codes_2$ISO_numeric)

NA_ISO_numeric_data <- numerical_codes_2[NA_ISO_numeric_logic, ] # 7
  
numerical_codes_2 <- numerical_codes_2[!NA_ISO_numeric_logic,]

# attach gadm ADM0 country code (gadm numeric) to age structure data using the numerical code
age_struct_data_ISO <- merge(age_struct_data, numerical_codes_2, 
                             by.x = "Country_code", 
                             by.y = "ISO_numeric", 
                             all.y = TRUE)

# all.y = T because I want to know for which countries there will be no age structure data

NA_age_strc_logic <- is.na(age_struct_data_ISO$Major_area_region_country_or_area)

NA_age_strc_data <- age_struct_data_ISO[NA_age_strc_logic,] # 49

# assign 0 to the each age band of countries where all age band are missing
age_struct_data_ISO[NA_age_strc_logic, numeric_columns] <- 0

# get the position of all the other missing values 
NA_some_age_band <- is.na(age_struct_data_ISO[, numeric_columns])

# assign a zero to all those values  
age_struct_data_ISO[, numeric_columns][NA_some_age_band] <- 0


# ---------------------------------------- Keep only one reference date per country


# Split data by gadm adm_0 code  
age_struct_data_ISO_by_code <- split(age_struct_data_ISO, age_struct_data_ISO$ID_0)

# Reorder records of each country according to decreasing reference date 
age_struct_data_ISO_by_code_ordered <- lapply(age_struct_data_ISO_by_code, 
                                                  function(x) x[order(x$Reference_date, decreasing = TRUE), ])

# For each country get the first record (which is now the most recent)
most_recent_years <- lapply(age_struct_data_ISO_by_code_ordered, "[", 1, , drop = FALSE)

most_recent_years_df <- do.call("rbind", most_recent_years)


# ---------------------------------------- reshape and subsetting


# Subset some columns of the df 
final_age_struct_data <- most_recent_years_df[, c("country", 
                                                  "ID_0", 
                                                  "Reference_date", 
                                                  numeric_columns)]

# Calculate total number of individuals in each country
tot_country_pop <- rowSums(final_age_struct_data[, numeric_columns])

# Calculate proportion of total population in each age band, per country
final_age_struct_data[, numeric_columns] <- final_age_struct_data[, numeric_columns] / tot_country_pop

# fix NaNs
final_age_struct_data[is.na(final_age_struct_data)] <- 0

# get Channel Islands age structure
Ch_Isl_age_struc_data <- subset(age_struct_data, Major_area_region_country_or_area == "Channel Islands" & Reference_date == 2015)[, c("Reference_date", numeric_columns)]

# fix NAs
Ch_Isl_age_struc_data[is.na(Ch_Isl_age_struc_data)] <- 0

# assign Channel Islands age structure to Jersey and Guernsey records
final_age_struct_data[final_age_struct_data$country == "Jersey", c("Reference_date", numeric_columns)] <- Ch_Isl_age_struc_data
final_age_struct_data[final_age_struct_data$country == "Guernsey", c("Reference_date", numeric_columns)] <- Ch_Isl_age_struc_data

# Sort by country name
final_age_struct_data <- final_age_struct_data [order(final_age_struct_data$country), ]


# ---------------------------------------- now remove the zero (!) (NEED TO FIX THIS CODE!) 


zero_records <- apply(final_age_struct_data[, numeric_columns], 1, sum) == 0

final_age_struct_data_mz <- final_age_struct_data[!zero_records, ]


# ---------------------------------------- save  


write.csv(final_age_struct_data_mz, 
          file.path(out_pt, out_nm), 
          row.names = FALSE)
