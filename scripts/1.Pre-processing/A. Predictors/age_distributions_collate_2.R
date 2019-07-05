

source(file.path("R", "prepare_datasets", "grouped_data_stats.R"))
source(file.path("R", "utility_functions.R"))


# define parameters -----------------------------------------------------------


dts_out_pt <- file.path("output", "datasets")

dts_out_nm <- "country_age_structure.csv"


# load data -------------------------------------------------------------------


age_struct_data <- read.csv(file.path("output", 
                                      "datasets", 
                                      "country_age_structure.csv"))


# pre processing --------------------------------------------------------------


numeric_columns <- grep("band", names(age_struct_data), value = TRUE)

age_bounds_num <- edit_age_band_tags(age_struct_data)

xx <- strsplit(age_bounds_num, "-")
zz <- lapply(xx, as.numeric)
yy <- vapply(zz, mean, numeric(1))

mean_ages <- apply(age_struct_data[, numeric_columns], 1, mean_grouped_data, yy)

n_c <- nrow(age_struct_data)

sd <- vapply(
  seq_len(n_c), 
  sd_grouped_data,
  numeric(1),
  age_dat = age_struct_data[, numeric_columns],
  mean_vals = mean_ages) 

age_struct_data$mean_age <- mean_ages

age_struct_data$sd_age <- sd


# save ------------------------------------------------------------------------


write_out_csv(age_struct_data, dts_out_pt, dts_out_nm)
