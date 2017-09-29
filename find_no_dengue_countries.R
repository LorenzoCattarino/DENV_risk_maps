
library(maptools) 
library(dplyr)


### Check FIRST word document for how to prepare datasets to import

  
# ---------------------------------------- load data
  
  
consensus_dat <- read.csv(file.path("data", "evidence-based_consensus_scores.csv"), 
                                    header = TRUE, 
                                    stringsAsFactors = FALSE)

world_shp_admin_1 <- readShapePoly(file.path("data", "shapefiles", "gadm28_levels.shp", "gadm28_adm1.shp"))

all_adm1 <- read.csv(file.path("output", "env_variables", "All_adm1_env_var.csv"), 
                     header = TRUE, 
                     stringsAsFactors = FALSE)
names(all_adm1)[names(all_adm1) == "country"] <- "NAME_0"
names(all_adm1)[names(all_adm1) == "name1"] <- "NAME_1"


# ---------------------------------------- fix the score column


consensus_dat$Evidence.consensus <- gsub("%", " ", consensus_dat$Evidence.consensus)

consensus_dat$Evidence.consensus <- as.numeric(consensus_dat$Evidence.consensus)


# ---------------------------------------- remove records I dont know what they are 


bad <- which(consensus_dat$Country == "U.S.A.-")

consensus_dat <- consensus_dat[-bad, ]


# ---------------------------------------- subset countries 


# keep Uruguay and Botswana in dengue country pool (Neil)
uru_bots_arg <- consensus_dat$Country %in% grep("Uruguay|Botswana|Entre Rios|Salta", consensus_dat$Country, value = TRUE)

# filter out most of the records based on consensus score
consensus_dat_2 <- subset(consensus_dat, Evidence.consensus > -40 | uru_bots_arg) 

# leave out France and Croatia
fra_cro <- consensus_dat_2$Country %in% grep("France|Croatia", consensus_dat_2$Country, value = TRUE)

consensus_dat_2 <- subset(consensus_dat_2, !fra_cro) 
                        


# =================================================================== 
#
#                                          start data cleaning
#
# ===================================================================


# ---------------------------------------- create two groups of countries
# 1) countries where dengue occurs in some admin units
# 2) countries where dengue occurs in all adm 1 units
 

indices_dot_nms <- grep("- ", consensus_dat_2$Country)

dot_nms <- consensus_dat_2[indices_dot_nms, "Country"]

no_dot_nms <- consensus_dat_2[-indices_dot_nms, "Country"]


# ---------------------------------------- split country names from adm1/adm2 names 


split_dot_nms <- strsplit(dot_nms, "- ")


# ---------------------------------------- create a df of group 1 countries


first_nms <- vapply(split_dot_nms, "[[", character(1), 1)

second_nms <- vapply(split_dot_nms, "[[", character(1), 2)

dat_1 <- data.frame(NAME_0 = first_nms, NAME_1 = second_nms, stringsAsFactors = FALSE)


# ---------------------------------------- scale adm2 records to adm 1


dat_1_by_c <- split(dat_1, as.factor(dat_1$NAME_0))

dat_1_by_c$Australia[1, "NAME_1"] <- "Queensland" 
dat_1_by_c$Australia <- dat_1_by_c$Australia[1, ]

dat_1_by_c$`U.S.A. Texas`[1, "NAME_1"] <- "Texas" 
dat_1_by_c$`U.S.A. Texas`[1, "NAME_0"] <- "United States" 
dat_1_by_c$`U.S.A. Texas` <- dat_1_by_c$`U.S.A. Texas`[1, ]

dat_1_by_c$`U.S.A. Florida`[1, "NAME_1"] <- "Florida" 
dat_1_by_c$`U.S.A. Florida`[1, "NAME_0"] <- "United States" 
dat_1_by_c$`U.S.A. Florida` <- dat_1_by_c$`U.S.A. Florida`[1, ]

dat_2 <- do.call("rbind", dat_1_by_c)


# ---------------------------------------- fix adm 1 names


# you have to make sure the adm0/adm1 names in dengue countries matches the adm0/adm1 names in the admin unit file 


dat_2[dat_2[,2] == "Buenos Aires df", "NAME_1"] <- "Ciudad de Buenos Aires"
dat_2[dat_2[,2] == "Cordoba", "NAME_1"] <- "CÃ³rdoba"
dat_2[dat_2[,2] == "Tucuman", "NAME_1"] <- "TucumÃ¡n"
dat_2[dat_2[,2] == "Entre Rios", "NAME_1"] <- "Entre RÃos"

dat_2[dat_2[,2] == "Easter Island", "NAME_1"] <- "ValparaÃso"

dat_2[dat_2[,2] == "Islamabad", "NAME_1"] <- "F.C.T."
dat_2[dat_2[,2] == "Khyber Pakhtunkhwa", "NAME_1"] <- "N.W.F.P."
dat_2[dat_2[,2] == "Sindh", "NAME_1"] <- "Sind"
dat_2[dat_2[,2] == "Tribal areas", "NAME_1"] <- "F.A.T.A."


# ---------------------------------------- remove India and Ururguay from group 1 countries 


indices_to_remove <- which(dat_2$NAME_0=="India" | dat_2$NAME_0=="Uruguay")
dat_2 <- dat_2[-indices_to_remove,]

  
# ---------------------------------------- join adm0/adm1 numerical codes


dat_2 <- left_join(dat_2, all_adm1[,c("ID_0","NAME_0","ID_1","NAME_1")])

adm0_adm1_values <- matrix(c(12,12,12,48,2,8,24,15), ncol = 2)

non_matched_pos <- apply(dat_2,1,anyNA)

dat_2[non_matched_pos, c("ID_0", "ID_1")] <- adm0_adm1_values


# ---------------------------------------- add India and Uruguay to group 2 countries  


no_dot_nms <- c(no_dot_nms, "India", "Uruguay")


# ---------------------------------------- fix country names


no_dot_nms[no_dot_nms == "Virgin Islands, British"] <- "British Virgin Islands"
no_dot_nms[no_dot_nms == "Congo (Democratic Republic of)"] <- "Democratic Republic of the Congo"
no_dot_nms[no_dot_nms == "Congo"] <- "Republic of Congo"
no_dot_nms[no_dot_nms == "Cote d'Ivoire"] <- "CÃ´te d'Ivoire"
no_dot_nms[no_dot_nms == "Lao People's Democratic Republic"] <- "Laos"
no_dot_nms[no_dot_nms == "Syrian Arab Republic"] <- "Syria"
no_dot_nms[no_dot_nms == "Tanzania (United Republic of)"] <- "Tanzania"
no_dot_nms[no_dot_nms == "The Gambia"] <- "Gambia"


# ---------------------------------------- join adm0/adm1 numerical codes

  
dat_3 <- data.frame(NAME_0 = no_dot_nms, stringsAsFactors = FALSE)

all_adm1_v2 <- all_adm1[!duplicated(all_adm1[,c("ID_0","NAME_0")]),]

dat_3 <- left_join(dat_3, all_adm1_v2[,c("ID_0","NAME_0")])

dat_3[dat_3$NAME_0 == "CÃ´te d'Ivoire", "ID_0"] <- 57

no_adm1_coun <- which(is.na(dat_3$ID_0))

dat_3 <- dat_3[-no_adm1_coun,]


# ---------------------------------------- combine together


dengue_countries <- bind_rows(dat_2, dat_3)

rownames(dengue_countries) <- NULL



# =================================================================== 
#
#                                          end data cleaning
#
# ===================================================================



# ---------------------------------------- remove countries not present in shepefiles (e.g., Netherlands Antilles) 


#dengue_countries_2 <- dengue_countries[country_name_in_shp_logical,]
dengue_countries_2 <- dengue_countries

# ---------------------------------------- get some objects

# get names of countries where there is dengue in all admin 1 regions 
whole_countries <- dengue_countries_2[is.na(dengue_countries_2$ID_1)==TRUE, "ID_0"]

# get name of countries where there is dengue only in specific admin 1 regions (and get also admin1 name)
specific_admin1_records <- dengue_countries_2[is.na(dengue_countries_2$ID_1)==FALSE,]

# add column for presence/absence to attribute table 
world_shp_admin_1@data <- cbind(world_shp_admin_1@data, dengue=rep(0,nrow(world_shp_admin_1@data)))


# ---------------------------------------- assign dengue presence/absence 


# give dengue presence to countries where there is dengue everywhere 
world_shp_admin_1@data[world_shp_admin_1@data$ID_0 %in% whole_countries, "dengue"] <- 1
  
# give dengue presence to countries where there is dengue in specific admin1 regions  
for (i in seq_len(nrow(specific_admin1_records)))
{
  country_name <- as.character(specific_admin1_records[i,"ID_0"])
  #cat("country name =", country_name, "\n")  
  
  admin1_name <- as.character(specific_admin1_records[i,"ID_1"])
  #cat("admin1 name =", admin1_name, "\n") 
  
  world_shp_admin_1@data[world_shp_admin_1@data$ID_0==country_name & world_shp_admin_1@data$ID_1==admin1_name, "dengue"] <- 1
}

# get ISO and country name from all admin 1 regions where there is no dengue 
no_dengue_shp_records <- world_shp_admin_1@data [world_shp_admin_1@data$dengue==0, c("ISO", "NAME_0")]
names(no_dengue_shp_records) <- c("country_code", "country")
no_dengue_shp_records <- no_dengue_shp_records[c(2,1)]
no_dengue_shp_records_unique <- no_dengue_shp_records[!duplicated(no_dengue_shp_records),]
no_dengue_shp_records_unique_sorted <- no_dengue_shp_records_unique[order(no_dengue_shp_records_unique$country), ]

# write out shapefile with the new attribute 
writeSpatialShape(world_shp_admin_1, file.path("data", "shapefiles", "gadm28_levels.shp", "gadm28_adm1_dengue_2"))

# Write out .csv as well
write.table(no_dengue_shp_records_unique_sorted,
            file.path("output", "datasets", "no_dengue_countries_2.csv"), row.names = FALSE, sep=",")
