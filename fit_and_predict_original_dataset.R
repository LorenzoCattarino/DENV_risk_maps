
library(h2o)
library(ggplot2)
library(ranger)
library(rgdal)
library(colorRamps)
library(lattice)
library(grid)

source(file.path("R", "random_forest", "fit_h2o_RF_and_make_predictions.R"))
source(file.path("R", "random_forest", "fit_ranger_RF_and_make_predictions.R"))
source(file.path("R", "prepare_datasets", "set_pseudo_abs_weights.R"))
source(file.path("R", "plotting", "quick_polygon_map.R"))


# define parameters -----------------------------------------------------------


parameters <- list(
  dependent_variable = "FOI",
  grid_size = 5,
  b = 0,
  c = 5,
  d = 1.6e6,
  pseudoAbs_value = -0.02,
  no_trees = 500,
  min_node_size = 20,
  all_wgt = 1,
  wgt_limits = c(1, 500),
  no_samples = 200,
  no_predictors = 9) 

out_pt <- file.path("figures", "original_dataset_fits")
out_name <- "admin1_map.png"

my_col <- matlab.like(100)


# define variables ------------------------------------------------------------


dependent_variable <- parameters$dependent_variable
pseudoAbsence_value <- parameters$pseudoAbs_value
no_predictors <- parameters$no_predictors
all_wgt <- parameters$all_wgt
no_trees <- parameters$no_trees
min_node_size <- parameters$min_node_size


# load data -------------------------------------------------------------------


foi_data <- read.csv(file.path("output", 
                               "foi", 
                               "All_FOI_estimates_linear_env_var_area_salje.csv"),
                     stringsAsFactors = FALSE)

predictor_rank <- read.csv(file.path("output", 
                                     "variable_selection", 
                                     "metropolis_hastings", 
                                     "exp_1", 
                                     "variable_rank_final_fits_exp_1.csv"),
                           stringsAsFactors = FALSE)

adm1_dataset <- read.csv(file.path("output", 
                                   "env_variables", 
                                   "all_adm1_env_var.csv"))

adm_shp <- readOGR(dsn = file.path("output", "shapefiles"), 
                   layer = "gadm28_adm1_eras",
                   stringsAsFactors = FALSE)

country_shp <- readOGR(dsn = file.path("output", "shapefiles"), 
                       layer = "gadm28_adm0_eras",
                       stringsAsFactors = FALSE)


# pre processing --------------------------------------------------------------


foi_data[foi_data$type == "pseudoAbsence", dependent_variable] <- pseudoAbsence_value

foi_data$new_weight <- all_wgt
pAbs_wgt <- get_sat_area_wgts(foi_data, parameters)
foi_data[foi_data$type == "pseudoAbsence", "new_weight"] <- pAbs_wgt

my_predictors <- predictor_rank$name[1:no_predictors]


# -----------------------------------------------------------------------------


# get training dataset (full dataset - no bootstrap)
training_dataset <- foi_data[, c(dependent_variable, my_predictors, "new_weight")]


# -----------------------------------------------------------------------------


# h2o.init()
# 
# RF_obj <- fit_h2o_RF(dependent_variable = dependent_variable,
#                      predictors = my_predictors,
#                      training_dataset = training_dataset,
#                      no_trees = no_trees,
#                      min_node_size = min_node_size,
#                      my_weights = "new_weight")
# 
# p_i <- make_h2o_predictions(
#   mod_obj = RF_obj,
#   dataset = training_dataset,
#   sel_preds = my_predictors)
# 
# foi_data$p_i <- p_i
# 
# h2o.shutdown(prompt = FALSE)


RF_obj <- fit_ranger_RF(dependent_variable = dependent_variable,
                        predictors = my_predictors,
                        training_dataset = training_dataset,
                        no_trees = no_trees,
                        min_node_size = min_node_size,
                        my_weights = "new_weight")


# make predictions of the data ------------------------------------------------


p_i <- make_ranger_predictions(
  mod_obj = RF_obj,
  dataset = training_dataset,
  sel_preds = my_predictors)

foi_data$p_i <- p_i

foi_data <- foi_data[foi_data$type != "pseudoAbsence",] 

corr_coeff <- round(cor(foi_data$FOI, foi_data$p_i), 3)


# scatterplot with corr coeff -------------------------------------------------


p <- ggplot(foi_data) +
  geom_point(aes(x = FOI, y = p_i), size = 1) +
  geom_abline(slope = 1, intercept = 0, linetype = 2) +
  coord_cartesian(xlim = c(0,0.06), ylim = c(0,0.06)) +
  geom_text(aes(x = 0.05, y = 0.01, label = paste0("r = ", corr_coeff)))

ggsave(file.path("figures", "test_fit_without_pop.png"), p, width = 15, height = 8, units = "cm")


# make global adm1 predictions ------------------------------------------------


admin <- make_ranger_predictions(
  mod_obj = RF_obj,
  dataset = adm1_dataset,
  sel_preds = my_predictors)

adm1_dataset$admin <- admin

adm1_dataset$admin[adm1_dataset$admin < 0] <- 0

adm_shp_pred <- merge(adm_shp, 
                      adm1_dataset[, c("ID_0", "ID_1", "admin")], 
                      by = c("ID_0", "ID_1"), 
                      all.x = TRUE)


# global adm1 map -------------------------------------------------------------


quick_polygon_map(adm_shp_pred, 
                  country_shp, 
                  my_col,
                  "admin", 
                  out_pt, 
                  out_name)
