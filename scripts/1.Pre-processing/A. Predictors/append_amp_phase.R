rm(list=ls())

if (!require("FT.amp.phase")) devtools::install_github("tinigarske/FT.amp.phase", 
                                                       auth_token = "8497ecd3449faefacf5b5868042f01e044ed834c")

# load R packages 
library(FT.amp.phase)

# load data 
dengue_point_countries <- read.csv (file.path("output", "datasets", "dengue_point_countries.csv"), header = TRUE, sep = ",")
no_dengue_countries <- read.csv(file.path("output", "datasets", "no_dengue_countries.csv"), header = TRUE, sep = ",")

# define some parameters 
no_covariate_countries <- c("GRL", "FRO", "ISL", "SJM", "GGY") # I could not fix the shapefiles for GGY, which is corrupted (has extra lines in the dbf)
FTs_data_names <- c("DayTemp", "EVI", "MIR", "NightTemp", "RFE")

# get vector of country codes for which FT variables have been extracted 
all_country_codes <- as.character(unlist(list(dengue_point_countries$ISO, no_dengue_countries$ISO)))
all_covariate_countries <- sort(all_country_codes[!all_country_codes %in% no_covariate_countries])

for (i in 1:length(FTs_data_names))
{
  file.type <- FTs_data_names[i]  
  cat("file type = ", file.type, "\n")
  
  for (j in 1:length(all_covariate_countries))
  {
    adm0 <- all_covariate_countries[j]
    cat("country code = ", adm0, "\n")
    
    FT <- read.table(file.path("data", "processed", "adm1", "FTs", paste0("FTfreqs_2007-2014_64ppyear_", file.type, "_adm1_", adm0, ".txt")), 
                     header = TRUE, sep = "\t", quote = "")
    amp.phase <- FTcoeff.to.amp.phase.all.modes(FT)
    
    FT <- cbind(FT, data.frame(amp.phase))
    write.table(FT, file.path("data", "processed", "adm1", "FTs", paste0("amp_phases_", file.type, "_adm1_", adm0, ".txt")),
                sep = "\t", row.names = FALSE, col.names = TRUE, quote = FALSE)
  }
}
