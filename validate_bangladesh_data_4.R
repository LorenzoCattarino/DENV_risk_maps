
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
  
dts_out_nm <- "observations_adm1_clean.txt" 


# load data -------------------------------------------------------------------


salje_data <- read.csv(file.path("output", 
                                 "seroprevalence",
                                 "salje",
                                 "observations_20km.csv"),
                       stringsAsFactors = FALSE)


# keep ID_1 unique salje points -----------------------------------------------


ave_foi <- group_by(salje_data, ID_1) %>%
  summarise(FOI = mean(FOI)) %>% 
  left_join(distinct(salje_data[setdiff(names(salje_data), "FOI")], ID_1, .keep_all = TRUE))

ave_foi <- ave_foi[, base_info]


# save dataset ----------------------------------------------------------------


write.table(ave_foi,
            file.path(dts_out_pt, dts_out_nm),
            col.names = TRUE,
            row.names = FALSE,
            sep = ",")
