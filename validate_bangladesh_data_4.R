
# average up the 70 Salje points within the 7 bangladesh admin units
# at the end of this code you will need to run the covariate extraction code


library(dplyr)


# define parameters -----------------------------------------------------------


base_info <- c("type", 
               "ID_0", 
               "ISO", 
               "country", 
               "ID_1", 
               "FOI", 
               "variance", 
               "latitude", 
               "longitude", 
               "reference", 
               "date")

dts_out_pt <- file.path("output", "seroprevalence", "salje")
  
dts_out_nm <- "observations_adm1.txt" 


# load data -------------------------------------------------------------------


foi_dataset <- read.csv(file.path("output", 
                                  "foi", 
                                  "All_FOI_estimates_linear_env_var_area.csv"),
                        stringsAsFactors = FALSE) 

salje_data <- read.csv(file.path("output", 
                                 "seroprevalence",
                                 "salje",
                                 "observations_20km.csv"),
                       stringsAsFactors = FALSE)


# pre processing --------------------------------------------------------------


salje_data$data_id <- seq_len(nrow(salje_data))
salje_data$type <- "serology"
salje_data$reference <- "Salje"
salje_data$date <- "2018"
salje_data$variance <- 0


# keep ID_1 unique salje points -----------------------------------------------


ave_foi <- group_by(salje_data, ID_1) %>%
  summarise(FOI = mean(foi)) %>% 
  left_join(distinct(salje_data, ID_1, .keep_all = T))

ave_foi <- ave_foi[, base_info]


# save dataset ----------------------------------------------------------------


write.table(ave_foi,
            file.path(dts_out_pt, dts_out_nm),
            col.names = TRUE,
            row.names = FALSE,
            sep = ",")
