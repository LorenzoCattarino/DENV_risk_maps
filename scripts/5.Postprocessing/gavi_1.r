library(dplyr)

source(file.path("R", "prepare_datasets", "average_up.r"))

dat <- readRDS(
  file.path("output", 
            "predictions_world", 
            "boot_model_20km_5", 
            "means", 
            "FOI_r_mean_all_squares_0_1667_deg_3.rds"))

age_struc <- read.csv(
  file.path("output",
            "datasets",
            "country_age_structure.csv"))
  
aggreg_dat <- average_up(dat, "ADM_0", "mean")

aggreg_dat_c_nms <- left_join(aggreg_dat, age_struc[,c("country","ID_0")], by = c("ADM_0" = "ID_0"))

write.csv(aggreg_dat_c_nms[,c("ADM_0", "country", "mean")], "mean_foi_by_country.csv")
