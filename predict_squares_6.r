# Take mean, sd and 95%CI of foi, R0 and burden measures, for each 20 km square 

source(file.path("R", "utility_functions.r"))
source(file.path("R", "prepare_datasets", "calculate_mean_across_fits.r"))


# ---------------------------------------- define parameters


model_tp <- "boot_model_20km_2"

in_path <- file.path("output", "predictions_world", model_tp)

out_path <- file.path("output", "predictions_world", model_tp, "means")

vars <- c("FOI")#, "R0_r", "I_inc", "C_inc") # when fitting the R0 get also "FOI_r"

no_fits <- 200

col_names <- as.character(seq_len(no_fits))

base_info <- c("cell", "lat.grid", "long.grid", "population", "ADM_0", "ADM_1", "ADM_2")


# ---------------------------------------- run


for (j in seq_along(vars)){
  
  my_var <- vars[j]
  
  root_name <- paste0(my_var, "_all_squares_0_1667_deg")
  
  if(j == 1) {
    
    dat <- readRDS(file.path(in_path, paste0(root_name, ".rds")))
    
    ret <- average_boot_samples_dim2(dat)

    out_name <- paste0(my_var, "_mean_all_squares_0_1667_deg.rds")
      
    write_out_rds(ret, out_path, out_name)
    
  } else {
    
    for (i in seq_len(9)){
      
      dat <- readRDS(file.path(in_path, sprintf("%s_%s%s", root_name, i, ".rds")))
      
      ret <- average_boot_samples_dim2(dat[, col_names])
      
      out_name <- sprintf("%s_%s%s", root_name, i, ".rds")
      
      ret2 <- cbind(dat[, base_info], ret) 
        
      write_out_rds(ret2, out_path, out_name)
      
    }
      
  }

}
