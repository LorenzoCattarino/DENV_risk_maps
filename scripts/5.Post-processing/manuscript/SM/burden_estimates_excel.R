

# Create Excel workbook with country estimates of infections, cases and 
# hospitalized cases in the same worksheet. 
# Estimates from a model with a different response variable for each worksheet.


library(xlsx)

source(file.path("R", "create_parameter_list.R"))


# define parameters -----------------------------------------------------------


extra_prms <- list(id = 2,
                   model_responses = "FOI",
                   burden_measures = c("infections", "cases", "hosp"),
                   baseline_scenario_ids = 4)   

  
# define variables ------------------------------------------------------------


parameters <- create_parameter_list(extra_params = extra_prms)

model_type <- paste0("model_", parameters$id)

in_path <- file.path("output", 
                     "predictions_world", 
                     "bootstrap_models", 
                     model_type)

burden_measures <- parameters$burden_measures

baseline_scenario_ids <- parameters$baseline_scenario_ids

model_responses <- parameters$model_responses

out_path <- file.path("output", "predictions_world", "bootstrap_models")
  
out_nm <- "burden_estimates.xlsx" 
  
  
# pre processing --------------------------------------------------------------


all_combs <- expand.grid(burden_measures, c("mean", "lCI", "uCI"))

all_combs <- all_combs[order(all_combs$Var1), ]

all_names <- paste(all_combs$Var1, all_combs$Var2, sep = "_") 

all_names <- c("country", all_names)


# start -----------------------------------------------------------------------


#for (i in seq_along(model_type)){                       

# cat("R0 assumption =", i, "\n")

my_model_type <- model_type

baseline_scenario_id <- baseline_scenario_ids

out_ls <- vector("list", length(burden_measures)) 

for (j in seq_along(burden_measures)){
  
  bur_meas <- burden_measures[j]
  
  cat("burden measure =", bur_meas, "\n")
  
  dat <- read.csv(file.path("output", 
                            "predictions_world", 
                            "bootstrap_models",
                            my_model_type,
                            "wolbachia",
                            paste0(bur_meas, "_by_country_", baseline_scenario_id, ".csv")),
                  stringsAsFactors = FALSE)
  
  dat_2 <- dat[, setdiff(names(dat), "country")]
  
  out_ls[[j]] <- dat_2[, c("mean", "lCI", "uCI")]
  
}

ret <- do.call("cbind", out_ls)
ret2 <- cbind(dat$country, ret)
colnames(ret2) <- all_names

sheet_name <- paste0(model_responses, "_model")

write.xlsx(ret2, 
           file = file.path(out_path, out_nm),
           sheetName = sheet_name, 
           row.names = FALSE,
           append = FALSE)
