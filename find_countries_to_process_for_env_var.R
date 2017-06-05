rm(list=ls())

# load data
all_countries_to_predict <- read.csv(file.path("output", "datasets", "all_admin_1_countries.csv"), 
                                     header = TRUE, sep = ",")
datapoint_countries <- read.csv(file.path("output", "datasets", "dengue_point_countries.csv"),
                                header = TRUE, sep= ",")
no_dengue_countries <- read.csv(file.path("output", "datasets","no_dengue_countries.csv"), 
                                header = TRUE, sep = ",")

countries_already_processed <- rbind(datapoint_countries, no_dengue_countries)

countries_already_processed_unique <- countries_already_processed[!duplicated(countries_already_processed), ]

countries_already_processed_logical <- all_countries_to_predict$ISO %in% countries_already_processed_unique$country_code

countries_still_to_process <- all_countries_to_predict[!countries_already_processed_logical,]

write.table(countries_still_to_process[order(countries_still_to_process$NAME_0),], 
            file.path("output", "datasets", "countries_still_to_process.csv"), row.names=FALSE, sep=",")
