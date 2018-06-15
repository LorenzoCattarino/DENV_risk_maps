

# load data -------------------------------------------------------------------


age_distr <- read.csv(file.path("output", 
                                "datasets",
                                "country_age_structure.csv"), 
                      header = TRUE) 



# seroprevalence --------------------------------------------------------------


age_distr <- age_distr[setdiff(names(age_distr), c("band_80_99", "band_85_99"))]

age_band_tgs <- grep("band", names(age_distr), value = TRUE)

age_bounds_num <- sub("^[^_]+_", "", age_band_tgs)

age_bounds_num_2 <- sub("_", "-",age_bounds_num)

names(age_distr)[names(age_distr) %in% age_band_tgs] <- age_bounds_num_2

xx <- strsplit(age_bounds_num_2, "-")
zz <- lapply(xx, as.numeric)
yy <- vapply(zz, mean, numeric(1))

get_sero <- function(i, j){
  1 - (exp(-4 * i * j))
}

pred_serop <- t(vapply(salje_data_2$foi, get_sero, numeric(length(yy)), yy))

BGD_age_struct <- as.matrix(age_distr[age_distr$country == "Bangladesh", age_bounds_num_2])

BGD_age_structure_all_points <- matrix(rep(BGD_age_struct, 69), ncol = 20, byrow = TRUE)

mean_pred_serop <- rowSums(BGD_age_structure_all_points * pred_serop)

salje_data_2$p_j <- mean_pred_serop

salje_data_2$o_j <- salje_data_2$nPos / salje_data_2$nAll
