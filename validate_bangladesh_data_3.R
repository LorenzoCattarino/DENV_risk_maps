
# calculate foi from Salje's seroprevalence values
# calculate correlation with our 20 km predictions

library(ggplot2)

source(file.path("R", "prepare_datasets", "calculate_seroprevalence_age.R"))
source(file.path("R", "utility_functions.R"))


# define parameters -----------------------------------------------------------


FOI_values <- seq(0, 0.2, by = 0.0002)

map_out_pt <- file.path("figures", "data", "salje")

dts_out_pt <- file.path("output", "seroprevalence", "salje")


# load data -------------------------------------------------------------------


age_distr <- read.csv(file.path("output", 
                                "datasets",
                                "country_age_structure.csv"), 
                      header = TRUE) 

salje_data <- read.csv(file.path("output", 
                                 "seroprevalence",
                                 "salje",
                                 "predictions_20km.csv"),
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

pred_serop <- t(vapply(FOI_values, get_sero, numeric(length(yy)), yy))

BGD_age_struct <- as.matrix(age_distr[age_distr$country == "Bangladesh", age_bounds_num_2])
BGD_age_structure_all_points <- matrix(rep(BGD_age_struct, length(FOI_values)), ncol = 20, byrow = TRUE)

mean_pred_serop <- rowSums(pred_serop * BGD_age_structure_all_points) 

look_up <- data.frame(x = FOI_values, y = mean_pred_serop)

henrik_sero <- salje_data$o_j
  
henrik_foi <- approx(look_up[, "y"], look_up[, "x"], xout = henrik_sero)$y

salje_data$foi <- henrik_foi 


# plot ------------------------------------------------------------------------


corr_coeff <- round(cor(salje_data$foi_sqr, salje_data$foi), 3)

dir.create(map_out_pt, FALSE, TRUE)

p <- ggplot() +
  geom_point(aes(x = foi, y = foi_sqr, colour = "red"), data = salje_data, size = 1) +
  scale_colour_identity(name = "", guide = "legend", labels = "salje") +
  geom_abline(slope = 1, intercept = 0, linetype = 2) +
  # geom_text(aes(x = FOI, y = p_i, label = ID_1), data = henriks_points, nudge_y = 0.0004) +
  geom_text(aes(x = 0.03, y = 0.01, label = paste0("r = ", corr_coeff))) +
  scale_x_continuous("observed 20 km FOI", limits = c(0, 0.045)) +
  scale_y_continuous("predicted 20 km FOI", limits = c(0, 0.045))

ggsave(file.path(map_out_pt, "correlation_20km_pred_vs_observations.png"), 
       p, 
       width = 15, 
       height = 8, 
       units = "cm")


# save ------------------------------------------------------------------------


write_out_csv(salje_data, dts_out_pt, "observations_20km.csv")
