# Tweak lat and long to make sure that each data point has a corresponding 20 km square


# define parameters -----------------------------------------------------------


fields <- c("type", 
            "ISO",
            "country", 
            "FOI", 
            "variance", 
            "longitude",
            "latitude", 
            "reference", 
            "date")

foi_out_pt <- file.path("output", "foi")

foi_out_nm <- "FOI_estimates_lon_lat_twk.csv"


# load data -------------------------------------------------------------------


original_FOI_estimates <- read.csv(file.path("output",
                                             "foi",
                                             "FOI_estimates_lon_lat.csv"),
                                   header = TRUE,
                                   stringsAsFactors = FALSE)

missing_squares <- read.csv(file.path("output",
                                      "EM_algorithm",
                                      "missing_squares_for_orginal_datapoints.csv"),
                            header = TRUE,
                            stringsAsFactors = FALSE)

xy_to_tweak <- c(325, 326, 276, 1, 290, 278, 260, 302, 303, 17, 272, 295, 281, 293, 286, 20, 299) 


# subset ----------------------------------------------------------------------


all_foi_estimates <- original_FOI_estimates[, fields]


# replace lon and lat --------------------------------------------------------- 


# missing_squares_jn <- dplyr::inner_join(missing_squares, tweaked_xy, by = "data_id")
# 
# new_xy <- missing_squares_jn[, c("longitude.y", "latitude.y")]
# 
# all_foi_estimates[xy_to_tweak, c("longitude", "latitude")] <- new_xy


# save ------------------------------------------------------------------------


write.csv(all_foi_estimates, 
          file.path(foi_out_pt, foi_out_nm), 
          row.names = FALSE)
