# load functions 
source(file.path("R", "prepare_datasets", "get_env_variables.r"))
source(file.path("R", "utility_functions.r"))


# ---------------------------------------- define parameters 


base_info <- c("type", "longitude", "latitude", "ISO", "ID_0", "ID_1", "FOI", "R_0")

population_var <- "pop"

altitude_var <- "altitude"

fourier_transform_elem <- c("const_term",	"Re0",	"Im0",	"Re1",	"Im1")

FTs_dt <- c("RFE", "DayTemp", "NightTemp", "EVI", "MIR")

LandCover_var <- paste("lct1_2012001", c(seq(0, 16, 1), 254, 255), sep = "_")

foi_out_pt <- file.path("output", "foi")
foi_out_nm <- "All_FOI_estimates_linear_env_var.csv"

pred_out_pt <- file.path("output", "datasets")
pred_out_nm <- "all_predictors.txt"


# ---------------------------------------- load data 


All_FOI_R0_estimates <- read.csv(
  file.path("output", "R_0", "All_R_0_estimates.csv"), 
  header = TRUE, 
  sep = ",", 
  stringsAsFactors = FALSE)

pseudo_absence_points <- read.csv(
  file.path("output", 
            "datasets", 
            "pseudo_absence_points_NUM_CODES_sub.csv"), 
  header = TRUE, 
  sep = ",", 
  stringsAsFactors = FALSE)


# ---------------------------------------- pre processing


pseudo_absence_points$FOI <- 0
pseudo_absence_points$R_0 <- 0
  
All_FOI_R0_estimates <- All_FOI_R0_estimates[, base_info]
pseudo_absence_points <- pseudo_absence_points[, base_info]

foi_data <- rbind(All_FOI_R0_estimates, pseudo_absence_points)

# Get number of variables to extract
number_of_variables <- length(c(altitude_var, LandCover_var)) + (length(c(population_var, fourier_transform_elem)) * length(FTs_dt))

# Convert to list
foi_data_ls <- df_to_list(foi_data, use_names = TRUE)


# ---------------------------------------- run 

#extract env variables 
extracted_var_values <- sapply(
  foi_data_ls, 
  get_env_variables, 
  no_vars = number_of_variables, 
  pop_vars = population_var,
  alt_vars = altitude_var, 
  FT_elements = fourier_transform_elem, 
  FT_data = FTs_dt, 
  LC_vars = LandCover_var, 
  admin_level = 1,
  my_path = file.path("data", "env_variables"))

foi_data_with_env_variables <- cbind(foi_data, t(extracted_var_values))

colnames(foi_data_with_env_variables) <- c(names(foi_data), altitude_var, 
                                           apply(expand.grid(c(population_var, fourier_transform_elem), FTs_dt), 
                                                 1, 
                                                 function(x) {paste(x[2],x[1], sep="_")}), LandCover_var)

### Remove redundant population columns (not very elegant)

pop_col_names <- grep("pop", colnames(foi_data_with_env_variables), value = TRUE)

foi_data_with_env_variables <- foi_data_with_env_variables[setdiff(names(foi_data_with_env_variables), pop_col_names[(length(pop_col_names):2)])]

pop_col_names <- grep("pop", colnames(foi_data_with_env_variables), value = FALSE)

colnames(foi_data_with_env_variables)[pop_col_names] <- "population"

foi_data_with_env_variables <- cbind(data_id = seq_len(nrow(foi_data_with_env_variables)), foi_data_with_env_variables)

FT_vars <- apply(expand.grid(c(fourier_transform_elem), FTs_dt), 
                 1, 
                 function(x) paste(x[2],x[1], sep="_"))

all_variables <- c(FT_vars, altitude_var, LandCover_var)

predictor_table <- data.frame(variable = all_variables)


# ---------------------------------------- save 


write.csv(foi_data_with_env_variables, 
          file.path(foi_out_pt, foi_out_nm), 
          row.names = FALSE)

write.table(predictor_table,
            file.path(pred_out_pt, pred_out_nm),
            row.names = FALSE, 
            sep = ",")
