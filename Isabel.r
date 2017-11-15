# Join foi predictions to admin unit 1

library(dplyr)

data <- read.csv(file.path("output", "foi", "All_FOI_estimates_linear_env_var.csv"))
  
adm_pred <- read.csv(file.path("output", "predictions_world", "boot_model_20km_cw", "adm_1_predictions.csv"))

adm_pred <- adm_pred[!duplicated(adm_pred[, c("ID_0", "ID_1")]), ]

all_data <- left_join(data[,1:9], adm_pred[, c("ID_0", "ID_1", "foi_mean", "foi_sd")])

write.csv(all_data, "data_and_pred_admin_level.csv", row.names = FALSE)
