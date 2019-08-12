
source(file.path("R", "utility_functions.R"))
source(file.path("R", "create_parameter_list.R"))


# define parameters ----------------------------------------------------------- 


extra_prms <- list(id = 4,
                   vars = c("continent", "mean", "lCI", "uCI"))


# define variables ------------------------------------------------------------


parameters <- create_parameter_list(extra_params = extra_prms)

model_type <- paste0("model_", parameters$id)

vars <- parameters$vars

out_pth <- file.path("output", 
                     "predictions_world",
                     "bootstrap_models")

out_nm <- "burden_by_continent_table.csv"


# load data ------------------------------------------------------------------- 


infections <- read.csv(file.path("output", 
                                 "predictions_world",
                                 "bootstrap_models",
                                 model_type,
                                 "wolbachia",
                                 "infections_by_continent_4.csv"), 
                       stringsAsFactors = FALSE) 

cases <- read.csv(file.path("output", 
                                 "predictions_world",
                                 "bootstrap_models",
                                 model_type,
                                 "wolbachia",
                                 "cases_by_continent_4.csv"), 
                       stringsAsFactors = FALSE) 

hosp <- read.csv(file.path("output", 
                            "predictions_world",
                            "bootstrap_models",
                            model_type,
                            "wolbachia",
                            "hosp_by_continent_4.csv"), 
                  stringsAsFactors = FALSE) 

baseline_total_wolbachia <- read.csv(file.path("output", 
                                 "predictions_world",
                                 "bootstrap_models",
                                 "baseline_total_wolbachia.csv"), 
                       stringsAsFactors = FALSE) 


# load data ------------------------------------------------------------------- 


infections_2 <- infections[, vars]
infections_3 <- rbind(infections_2,
                   cbind(continent = "Total", baseline_total_wolbachia[1, c("mean", "lCI", "uCI")]))

cases_2 <- cases[, vars]
cases_3 <- rbind(cases_2,
                      cbind(continent = "Total", baseline_total_wolbachia[2, c("mean", "lCI", "uCI")]))

hosp_2 <- hosp[, vars]
hosp_3 <- rbind(hosp_2,
                 cbind(continent = "Total", baseline_total_wolbachia[3, c("mean", "lCI", "uCI")]))

all <- rbind(infections_3,
             cases_3,
             hosp_3)

write_out_csv(all, out_pth, out_nm, row.names = FALSE)
