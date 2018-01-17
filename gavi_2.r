library(dplyr)

source(file.path("R", "utility_functions.r"))


# define parameters -----------------------------------------------------------


no_fits <- 200

base_info <- c("cell", "population", "ADM_0") #"lat.grid", "long.grid", 
  
wanted_info <- c("FOI", "R0_1", "R0_2", "R0_3", "I_FOI", "C_FOI", "I_R0_3", "C_R0_3")
  
out_pth <- file.path("output", "predictions_world", "predictions_for_vaccine_analysis")
  
  
# load data -------------------------------------------------------------------


foi_values <- readRDS(
  file.path("output", "predictions_world", "boot_model_20km_2", "FOI_all_squares.rds"))

R0_1_values <- readRDS(
  file.path("output", "predictions_world", "boot_model_20km_3", "FOI_all_squares.rds"))

R0_2_values <- readRDS(
  file.path("output", "predictions_world", "boot_model_20km_4", "FOI_all_squares.rds"))

R0_3_values <- readRDS(
  file.path("output", "predictions_world", "boot_model_20km_5", "FOI_all_squares.rds"))

I_foi <- readRDS(
  file.path("output", "predictions_world", "boot_model_20km_2", "I_num_all_squares_1.rds"))

C_foi <- readRDS(
  file.path("output", "predictions_world", "boot_model_20km_2", "C_num_all_squares_1.rds"))

I_R0_3 <- readRDS(
  file.path("output", "predictions_world", "boot_model_20km_5", "I_num_all_squares_3.rds"))

C_R0_3 <- readRDS(
  file.path("output", "predictions_world", "boot_model_20km_5", "C_num_all_squares_3.rds"))

sqr_covariates <- readRDS(
  file.path("output", "env_variables", "all_squares_env_var_0_1667_deg.rds"))

R0_1_values <- as.data.frame(R0_1_values)

R0_2_values <- as.data.frame(R0_2_values)

R0_3_values <- as.data.frame(R0_3_values)

I_foi <- as.data.frame(I_foi)

C_foi <- as.data.frame(C_foi)

I_R0_3 <- as.data.frame(I_R0_3)

C_R0_3 <- as.data.frame(C_R0_3)


# ---------------------------------------------


my_ncol<- length(c(base_info, wanted_info[1:4]))
my_nrow <- nrow(sqr_covariates)
  
for (i in seq_len(no_fits)){
  
  cat("Bootstrap sample =", i, "\n")
  
  dat_mat <- setNames(as.data.frame(matrix(0, nrow = my_nrow, ncol = my_ncol)),
                      nm = c(base_info, wanted_info[1:4]))
  
  dat_mat[, base_info] <- sqr_covariates[, base_info]
    
  dat_mat[, wanted_info[1:4]] <- cbind(foi_values[, i],
                                  R0_1_values[, i],
                                  R0_2_values[, i],
                                  R0_3_values[, i])
  
  dat_mat <- left_join(dat_mat, I_foi[, c("cell", as.character(i))])
  
  names(dat_mat)[names(dat_mat) == as.character(i)] <- wanted_info[5]
  
  dat_mat <- left_join(dat_mat, C_foi[, c("cell", as.character(i))])
  
  names(dat_mat)[names(dat_mat) == as.character(i)] <- wanted_info[6]
  
  dat_mat <- left_join(dat_mat, I_R0_3[, c("cell", as.character(i))])
  
  names(dat_mat)[names(dat_mat) == as.character(i)] <- wanted_info[7]
  
  dat_mat <- left_join(dat_mat, C_R0_3[, c("cell", as.character(i))])
  
  names(dat_mat)[names(dat_mat) == as.character(i)] <- wanted_info[8]
  
  write_out_rds(dat_mat, out_pth, paste0("boot_sample_", i, ".rds"))

}

#test5 <- readRDS("output/predictions_world/predictions_for_vaccine_analysis/boot_sample_5.rds")
library(rgdal)

adm_0 <- readOGR(dsn = file.path("data", "shapefiles", "gadm28_levels.shp"), layer = "gadm28_adm0")
#adm_1 <- readOGR(dsn = file.path("data", "shapefiles", "gadm28_levels.shp"), layer = "gadm28_adm1")

adm_0_sub <- adm_0@data[!duplicated(adm_0@data[, c("ID_0", "NAME_ENGLI")]), c("ID_0", "NAME_ENGLI")]
#adm_1_sub <- adm_1@data[!duplicated(adm_1@data[, c("ID_0", "NAME_0")]), c("ID_0", "NAME_0")]

names(adm_0_sub)[names(adm_0_sub) == "ID_0"] <- "ADM_0"
write.csv(adm_0_sub, "adm0_country_names.csv", row.names = FALSE)
