# Extracts age structure and birth rates from source csv files
# (SOURCE: World Population Prospects - Population Division - United Nations)

# load packages
library(dplyr)


# define parameters ----------------------------------------------------------- 


col_classes <- c("integer", rep("character", 3), rep("integer", 2), rep("numeric", 22))

col_classes_br <- c("integer", rep("character", 3), "integer", "character", rep("integer", 2), rep("numeric", 17))

out_pt <- file.path("output", "datasets")

out_nm <- "country_age_structure.csv"

br_col_names <- c("Births", "Crude_birth_rate")


# load data ------------------------------------------------------------------- 


age_struct_data <- read.csv(file.path("data",
                                      "population",
                                      "WPP2015_POP_F07_1_POPULATION_BY_AGE_BOTH_SEXES.csv"), 
                            colClasses = col_classes, na.strings = "…")

birth_rates <- read.csv(file.path("data",
                                  "population",
                                  "WPP2019_INT_F01_ANNUAL_DEMOGRAPHIC_INDICATORS.csv"), 
                        colClasses = col_classes_br, na.strings = "…")

numerical_codes <- read.csv(file.path("data", "country_codes.csv"), 
                            stringsAsFactors = FALSE) 

country_border_shp_fl <- rgdal::readOGR(dsn = file.path("data", 
                                                        "shapefiles", 
                                                        "gadm28_levels.shp"), 
                                        layer = "gadm28_adm0", 
                                        stringsAsFactors = FALSE)


# pre processing --------------------------------------------------------------
  

birth_rates[, br_col_names[1]] <- birth_rates[, br_col_names[1]] * 1000

numeric_columns <- grep("band", names(age_struct_data), value = TRUE)

numeric_columns <- numeric_columns[!numeric_columns %in% c("band_80_99", "band_85_99", "band_100_plus")]

age_struct_data <- age_struct_data[, setdiff(colnames(age_struct_data), c("band_80_99", "band_85_99", "band_100_plus"))]
  
age_struct_data[, numeric_columns] <- age_struct_data[, numeric_columns] * 1000 

all_ID_0_ISOs <- country_border_shp_fl@data[, c("ISO", "ID_0", "NAME_ENGLI")]  

# join GADM country code to UN country code
numerical_codes_2 <- left_join(numerical_codes, all_ID_0_ISOs, 
                               by = c("ISO_alpha" = "ISO")) 

# join GADM country code to age structure data using the UN country code
age_struct_data_ISO <- left_join(age_struct_data, numerical_codes_2, 
                                 by = c("Country_code" = "ISO_numeric")) 


# Keep only the latest record per country -------------------------------------


age_struct_data_ISO_by_code <- split(age_struct_data_ISO, age_struct_data_ISO$ID_0)

age_struct_data_ISO_by_code_ordered <- lapply(age_struct_data_ISO_by_code, 
                                                 function(x) x[order(x$Reference_date, decreasing = TRUE), ])

most_recent_years <- lapply(age_struct_data_ISO_by_code_ordered, "[", 1, , drop = FALSE)

most_recent_years_df <- do.call("rbind", most_recent_years)


# get the birth rates ---------------------------------------------------------


# 04072019 LC
# latest age structure record was from 2015 and that was the Landscan population
# layer I then used. So, get birth rates from 2015.

birth_rates <- birth_rates[birth_rates$Reference_date == 2015, ]
  
# join to birth rates
most_recent_years_df_2 <- left_join(most_recent_years_df, birth_rates[, c("Country_code", br_col_names)], 
                                    by = "Country_code") 


# reshape and subsetting ------------------------------------------------------ 


final_age_struct_data <- most_recent_years_df_2[, c("country", 
                                                    "ID_0", 
                                                    "Reference_date", 
                                                    numeric_columns,
                                                    br_col_names)]

tot_country_pop <- rowSums(final_age_struct_data[, numeric_columns])

final_age_struct_data[, numeric_columns] <- final_age_struct_data[, numeric_columns] / tot_country_pop

final_age_struct_data <- final_age_struct_data [order(final_age_struct_data$country), ]

final_age_struct_data = rename(final_age_struct_data, birth_rate = Crude_birth_rate)


# save ------------------------------------------------------------------------ 


            
write.csv(final_age_struct_data, 
          file.path(out_pt, out_nm), 
          row.names = FALSE)
