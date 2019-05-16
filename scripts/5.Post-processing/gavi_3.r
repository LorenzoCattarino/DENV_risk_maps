library(dplyr)

source(file.path("R", "utility_functions.r"))


# define parameters -----------------------------------------------------------


base_info <- c("cell", "population", "ADM_0") #"lat.grid", "long.grid", 

wanted_info <- c("FOI", "R0_1", "R0_2", "R0_3", "I_FOI", "C_FOI", "I_R0_3", "C_R0_3")

out_pth <- file.path("output", "predictions_world", "predictions_for_vaccine_analysis")


# load data -------------------------------------------------------------------


foi_values <- readRDS(
  file.path("output", "predictions_world", "FOI_best_model", "FOI_best_all_squares.rds"))

R0_1_values <- readRDS(
  file.path("output", "predictions_world", "R0_1_best_model", "R0_r_best_all_squares_1.rds"))

R0_2_values <- readRDS(
  file.path("output", "predictions_world", "R0_2_best_model", "R0_r_best_all_squares_2.rds"))

R0_3_values <- readRDS(
  file.path("output", "predictions_world", "R0_3_best_model", "R0_r_best_all_squares_3.rds"))

I_foi <- readRDS(
  file.path("output", "predictions_world", "FOI_best_model", "I_num_best_all_squares_1.rds"))

C_foi <- readRDS(
  file.path("output", "predictions_world", "FOI_best_model", "C_num_best_all_squares_1.rds"))

I_R0_3 <- readRDS(
  file.path("output", "predictions_world", "R0_3_best_model", "I_num_best_all_squares_3.rds"))

C_R0_3 <- readRDS(
  file.path("output", "predictions_world", "R0_3_best_model", "C_num_best_all_squares_3.rds"))

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


my_ncol <- length(c(base_info, wanted_info[1:4]))
my_nrow <- nrow(sqr_covariates)

dat_mat <- setNames(as.data.frame(matrix(0, nrow = my_nrow, ncol = my_ncol)),
                    nm = c(base_info, wanted_info[1:4]))

dat_mat[, base_info] <- sqr_covariates[, base_info]

dat_mat[, wanted_info[1:4]] <- cbind(foi_values[, "best"],
                                     R0_1_values[, "best"],
                                     R0_2_values[, "best"],
                                     R0_3_values[, "best"])

dat_mat <- left_join(dat_mat, I_foi[, c("cell", "best")])

names(dat_mat)[names(dat_mat) == "best"] <- wanted_info[5]

dat_mat <- left_join(dat_mat, C_foi[, c("cell", "best")])

names(dat_mat)[names(dat_mat) == "best"] <- wanted_info[6]

dat_mat <- left_join(dat_mat, I_R0_3[, c("cell", "best")])

names(dat_mat)[names(dat_mat) == "best"] <- wanted_info[7]

dat_mat <- left_join(dat_mat, C_R0_3[, c("cell", "best")])

names(dat_mat)[names(dat_mat) == "best"] <- wanted_info[8]

write_out_rds(dat_mat, out_pth, "best_fit_model.rds")

test <- readRDS("output/predictions_world/predictions_for_vaccine_analysis/best_fit_model.rds")
