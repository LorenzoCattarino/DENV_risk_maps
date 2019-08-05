test1 <- readRDS("output/predictions_world/bootstrap_models/model_4/C_num_wolbachia_4.rds")
test2 <- readRDS("output/predictions_world/bootstrap_models/model_4/C_num_wolbachia_4_fixed.rds")
col_ids <- as.character(1:200)

sd(test1[270000,col_ids])
sd(test2[270000,col_ids])
