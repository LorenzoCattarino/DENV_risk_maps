rm(list = ls())

library(plyr)

# load data
dengue_dataset <- read.csv(file.path("data", 
                                     "foi", 
                                     "All_FOI_estimates_linear_env_var.csv"),
                           stringsAsFactors = FALSE) 

aedes_generations <- read.csv(file.path("output", 
                                        "datasets", 
                                        "aedes_generations.csv"), 
                              stringsAsFactors = FALSE)

aedes_generations <- aedes_generations[c("ISO", "NAME_1", "layer")]

names(aedes_generations) <- c("country_code", "adm1", "layer")

full_dataset <- join(dengue_dataset, aedes_generations, 
                      by = c("country_code", "adm1"))
