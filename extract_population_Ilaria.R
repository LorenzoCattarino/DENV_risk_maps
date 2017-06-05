rm(list = ls())

library(dplyr)

adm_levels <- c(1, 2)

# dataset for predictions 
prediction_datasets <- lapply(adm_levels, function(x){
  read.csv(file.path("output", 
                     "env_variables", 
                     paste0("All_adm", x, "_env_var.csv")))})


# ---------------------------------------- adm 1


adm1_dataset <- prediction_datasets[[1]]

by_ID_0_country <- adm1_dataset %>% group_by(ID_0, country)

pop_mean <- by_ID_0_country %>% summarise(population = sum(population, na.rm = TRUE))
pop_mean_df <- as.data.frame(pop_mean)

write.table(pop_mean_df, 
            file.path("output", 
                      "env_variables", 
                      "contry_population.txt"),
            row.names = FALSE,
            sep = ",")


# ---------------------------------------- adm 2


adm2_dataset <- prediction_datasets[[2]]

adm2_dataset <- subset(adm2_dataset, adm0 == 33)

my_fileds <- c("ISO",
               "adm0", 
               "country", 
               "adm1", 
               "name1", 
               "adm2", 
               "name2", 
               "population")

adm2_dataset <- adm2_dataset[, my_fileds]

names(adm2_dataset)[c(2,4,6)] <- c("ID_0", "ID_1", "ID_2")

write.table(adm2_dataset, 
            file.path("output", 
                      "env_variables", 
                      "adm2_BRA_population.txt"),
            row.names = FALSE,
            sep = ",")
