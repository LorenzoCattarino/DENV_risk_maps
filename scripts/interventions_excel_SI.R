

# Create Excel workbook with country estimates of infections, cases and 
# hospitalized cases in the same worksheet. 
# Estimates from a model with a different response variable for each worksheet.


library(xlsx)


# define parameters -----------------------------------------------------------


parameters <- list(
  id = c(21, 22, 23, 24),
  model_responses = c("FOI", "R0_1", "R0_2", "R0_3"),
  no_samples = 200,
  burden_measures = c("infections", "cases", "hosp"),
  baseline_scenario_ids = c(4, 1, 2, 3))   

  
# define variables ------------------------------------------------------------


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

  
for (i in seq_along(model_type)){                       
  
  cat("R0 assumption =", i, "\n")
  
  my_model_type <- model_type[i]
  
  baseline_scenario_id <- baseline_scenario_ids[i]
  
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
  
  sheet_name <- paste0(model_responses[i], "_model")
    
  if (i == 1) {
    
    write.xlsx(ret2, 
               file = file.path(out_path, out_nm),
               sheetName = sheet_name, 
               row.names = FALSE,
               append = FALSE)
    
  } else {
    
    write.xlsx(ret2, 
               file = file.path(out_path, out_nm),
               sheetName = sheet_name,
               row.names = FALSE,
               append = TRUE)
    
  }

}
