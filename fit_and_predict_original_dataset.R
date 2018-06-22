
library(h2o)
library(ggplot2)

source(file.path("R", "random_forest", "fit_h2o_RF_and_make_predictions.R"))
source(file.path("R", "prepare_datasets", "set_pseudo_abs_weights.R"))


# define parameters -----------------------------------------------------------


dependent_variable <- "FOI"
pseudoAbsence_value <- -0.02
no_trees <- 500
min_node_size <- 20
all_wgt <- 1
wgt_limits <- c(1, 500)


# load data -------------------------------------------------------------------


foi_data <- read.csv(file.path("output", 
                               "foi", 
                               "All_FOI_estimates_linear_env_var_area.csv"),
                     stringsAsFactors = FALSE)

predictor_rank <- read.csv(file.path("output", 
                                     "variable_selection", 
                                     "metropolis_hastings", 
                                     "exp_1", 
                                     "variable_rank_final_fits_exp_1.csv"),
                           stringsAsFactors = FALSE)


# pre processing --------------------------------------------------------------


foi_data[foi_data$type == "pseudoAbsence", dependent_variable] <- pseudoAbsence_value

foi_data$new_weight <- all_wgt
pAbs_wgt <- get_area_scaled_wgts(foi_data, wgt_limits)
foi_data[foi_data$type == "pseudoAbsence", "new_weight"] <- pAbs_wgt

foi_data$log_pop_den <- log(1 + foi_data$pop_den)

my_predictors <- predictor_rank$name[1:9]

# my_predictors <- c(my_predictors, "log_pop_den")


# -----------------------------------------------------------------------------


# get training dataset (full dataset - no bootstrap)
training_dataset <- foi_data[, c(dependent_variable, my_predictors, "new_weight")]


# -----------------------------------------------------------------------------


h2o.init()

RF_obj <- fit_h2o_RF(dependent_variable = dependent_variable, 
                     predictors = my_predictors, 
                     training_dataset = training_dataset, 
                     no_trees = no_trees, 
                     min_node_size = min_node_size,
                     my_weights = "new_weight")

p_i <- make_h2o_predictions(
  mod_obj = RF_obj, 
  dataset = training_dataset, 
  sel_preds = my_predictors)

foi_data$p_i <- p_i
  
h2o.shutdown(prompt = FALSE)

foi_data <- foi_data[foi_data$type != "pseudoAbsence",] 

corr_coeff <- round(cor(foi_data$FOI, foi_data$p_i), 3)

p <- ggplot(foi_data) +
  geom_point(aes(x = FOI, y = p_i), size = 1) +
  geom_abline(slope = 1, intercept = 0, linetype = 2) +
  coord_cartesian(xlim = c(0,0.06), ylim = c(0,0.06)) +
  geom_text(aes(x = 0.05, y = 0.01, label = paste0("r = ", corr_coeff)))

ggsave(file.path("figures", "test_fit_without_pop.png"), p, width = 15, height = 8, units = "cm")
