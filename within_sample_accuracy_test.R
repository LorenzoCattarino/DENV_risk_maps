
library(h2o)
library(ggplot2)

source(file.path("R", "random_forest", "functions_for_fitting_h2o_RF_and_making_predictions.r"))


# -----------------------------------------------------------------------------


dependent_variable <- "FOI"
pseudoAbsence_value <- -0.02
no_trees <- 500
min_node_size <- 20
all_wgt <- 1
wgt_limits <- c(1, 500)


# -----------------------------------------------------------------------------


foi_data <- read.csv(
  file.path("output", "foi", "All_FOI_estimates_linear_env_var_area.csv"),
  stringsAsFactors = FALSE)

predictor_rank <- read.csv(
  file.path("output", 
            "variable_selection", 
            "metropolis_hastings", 
            "exp_1", 
            "variable_rank_final_fits_exp_1.csv"),
  stringsAsFactors = FALSE)

my_predictors <- predictor_rank$variable[1:18]

foi_data[foi_data$type == "pseudoAbsence", dependent_variable] <- pseudoAbsence_value

foi_data$new_weight <- all_wgt
pAbs_wgt <- get_area_scaled_wgts(foi_data, wgt_limits)
foi_data[foi_data$type == "pseudoAbsence", "new_weight"] <- pAbs_wgt


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

p <- ggplot(foi_data) +
  geom_point(aes(x = FOI, y = p_i), size = 1) +
  geom_abline(slope = 1, intercept = 0, linetype = 2) 

ggsave(file.path("figures", "test_fit.png"), p, width = 15, height = 8, units = "cm")

cor(foi_data$FOI, foi_data$p_i)
