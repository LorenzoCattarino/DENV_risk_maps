

# define parameters -----------------------------------------------------------


FOI_values <- seq(0, 0.2, by = 0.0002)



# load data -------------------------------------------------------------------


age_distr <- read.csv(file.path("output", 
                                "datasets",
                                "country_age_structure.csv"), 
                      header = TRUE) 

salje_data <- read.csv(file.path("output", 
                                 "seroprevalence",
                                 "ProportionPositive_bangladesh_salje_sqr_pred.csv"),
                       stringsAsFactors = FALSE)


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

pred_serop <- t(vapply(FOI_values, get_sero, numeric(length(yy)), yy))

# BGD_age_struct <- as.matrix(age_distr[age_distr$country == "Bangladesh", age_bounds_num_2])
# BGD_age_structure_all_points <- matrix(rep(BGD_age_struct, 69), ncol = 20, byrow = TRUE)

mean_pred_serop <- rowMeans(pred_serop)

look_up <- data.frame(x = FOI_values, y = mean_pred_serop)

henrik_sero <- salje_data$o_j
  
henrik_foi <- approx(look_up[, "y"], look_up[, "x"], xout = henrik_sero)$y

salje_data$foi <- henrik_foi 
