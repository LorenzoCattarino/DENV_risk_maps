foi_preds <- readRDS(file.path("output", 
                               "predictions_world", 
                               "bootstrap_models", 
                               "model_21",
                               "response.rds"))

resp_red <- readRDS(file.path("output", 
                              "predictions_world", 
                              "bootstrap_models", 
                              "model_22",
                              "response_r_wolbachia_1.rds"))

trans_red <- readRDS(file.path("output", 
                               "predictions_world", 
                               "bootstrap_models", 
                               "model_22",
                               "transformed_r_wolbachia_1.rds"))

cases <- readRDS(file.path("output", 
                           "predictions_world", 
                           "bootstrap_models", 
                           "model_22",
                           "C_num_wolbachia_1.rds"))

cases_fixed <- readRDS(file.path("output", 
                           "predictions_world", 
                           "bootstrap_models", 
                           "model_22",
                           "C_num_wolbachia_1_fixed.rds"))


infections <- readRDS(file.path("output", 
                                "predictions_world", 
                                "bootstrap_models", 
                                "model_23",
                                "I_num_wolbachia_2.rds"))

pxl_dataset <- readRDS(file.path("output",
                         "EM_algorithm",
                         "best_fit_models",
                         "env_variables",
                         "env_vars_20km_2.rds"))

transformed_1_r_wolbachia_4 <- readRDS(file.path("output",
                                                 "predictions_world",
                                                 "bootstrap_models",
                                                 "model_21",
                                                 "transformed_1_r_wolbachia_4.rds"))
