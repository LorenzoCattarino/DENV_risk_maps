rm(list=ls())

# load packages
library(maptools) 

# load functions
source(file.path("R", "prepare_datasets", "functions_for_finding_no_dengue_countries.R"))

## Check FIRST word document for how to prepare datasets to import

# load data 
dengue_presence_records <- read.csv (file.path("data", "evidence-based_consensus_scores.csv"), header = TRUE, sep = ",")
world_shp_admin_1 <- readShapePoly(file.path("data", "shapefiles", "gadm28_levels.shp", "gadm28_adm1.shp"))

# convert to factor 
dengue_presence_records$Country <- as.character(dengue_presence_records$Country)

# split country and admin1 names 
split_country_and_admin1_names <- strsplit(dengue_presence_records[,"Country"], " - ")

# get country names 
country_names <- do.call("rbind", lapply(split_country_and_admin1_names, "[[", 1))

# get admin1 names 
admin1_names <- sapply(split_country_and_admin1_names, get_admin1_names)

# combine in df 
dengue_presence_countries <- data.frame(NAME_0=country_names, NAME_1=admin1_names)

# remove countries not present in shepfiles (e.g., Netherlands Antilles) 
country_name_in_shp_logical <- apply(dengue_presence_countries, 1, check_country_name_in_shp, world_shp_admin_1, "NAME_0")
dengue_presence_countries_2 <- dengue_presence_countries[country_name_in_shp_logical,]

# get names of countries where there is dengue in all admin 1 regions 
whole_countries <- dengue_presence_countries_2[is.na(dengue_presence_countries_2$NAME_1)==TRUE, "NAME_0"]

# get name of countries where there is dengue only in specific admin 1 regions (and get also admin1 name)
specific_admin1_records <- dengue_presence_countries_2[is.na(dengue_presence_countries_2$NAME_1)==FALSE,]

# add column for presence/absence to attribute table 
world_shp_admin_1@data <- cbind(world_shp_admin_1@data, dengue=rep(0,nrow(world_shp_admin_1@data)))

# give dengue presence to countries where there is dengue everywhere 
world_shp_admin_1@data[world_shp_admin_1@data$NAME_0 %in% whole_countries, "dengue"] <- 1
  
# give dengue presence to countries where there is dengue in specific admin1 regions  
for (i in 1:nrow(specific_admin1_records))
{
  country_name <- as.character(specific_admin1_records[i,"NAME_0"])
  #cat("country name = ", country_name, "\n")  
  
  admin1_name <- as.character(specific_admin1_records[i,"NAME_1"])
  #cat("admin1 name = ", admin1_name, "\n") 
  
  world_shp_admin_1@data[world_shp_admin_1@data$NAME_0==country_name & world_shp_admin_1@data$NAME_1==admin1_name, "dengue"] <- 1
}

# get ISO and country name from all admin 1 regions where there is no dengue 
no_dengue_shp_records <- world_shp_admin_1@data [world_shp_admin_1@data$dengue==0, c("ISO", "NAME_0")]
names(no_dengue_shp_records) <- c("country_code", "country")
no_dengue_shp_records <- no_dengue_shp_records[c(2,1)]
no_dengue_shp_records_unique <- no_dengue_shp_records[!duplicated(no_dengue_shp_records),]
no_dengue_shp_records_unique_sorted <- no_dengue_shp_records_unique[order(no_dengue_shp_records_unique$country), ]

# write out shapefile with the new attribute 
writeSpatialShape(world_shp_admin_1, file.path("data","shapefiles", "gadm28_levels.shp", "gadm28_adm1_dengue"))

# Write out .csv as well
write.table(no_dengue_shp_records_unique_sorted,
            file.path("output", "datasets", "no_dengue_countries.csv"), row.names = FALSE, sep=",")
