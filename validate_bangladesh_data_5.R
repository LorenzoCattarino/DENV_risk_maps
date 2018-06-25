
# include the 7 Salje points into our dataset and save 

source(file.path("R", "utility_functions.R"))


# define parameters -----------------------------------------------------------


dts_out_pt <- file.path("output", "foi")

dts_out_nm <- "All_FOI_estimates_linear_env_var_area_salje.csv"


# load data -------------------------------------------------------------------


foi_dataset <- read.csv(file.path("output", 
                                  "foi", 
                                  "All_FOI_estimates_linear_env_var_area.csv"),
                        stringsAsFactors = FALSE) 

salje_data <- read.csv(file.path("output", 
                                 "seroprevalence",
                                 "salje",
                                 "observations_adm1.csv"),
                       stringsAsFactors = FALSE)


# pre process -----------------------------------------------------------------


foi_dataset <- foi_dataset[setdiff(names(foi_dataset), c("R0_1", "R0_2", "R0_3"))]

all_foi <- rbind(foi_dataset, salje_data)

write_out_csv(all_foi, dts_out_pt, dts_out_nm)
