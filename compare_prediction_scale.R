library(dplyr)

source(file.path("R", "utility_functions.r"))
source(file.path("R", "prepare_datasets", "average_up.r"))


# ---------------------------------------- define parameters 


model_type <- "best_model_20km_cw"
  
mod_tp <- substr(model_type, 1, 4)

out_name <- paste0("prediction_scale_comparison_", model_type, ".csv")

var_names_pxl_dts <- "mean_pred" #, "sd_pred", "low_perc", "up_perc")

out_path <- file.path(
  "output",
  "predictions",
  model_type)

if (mod_tp == "boot") {
  
  var_names_sqr_dts_train <- "mean_train" 
  var_names_sqr_dts_test <- "mean_test" 
  
} else {
  
  var_names_sqr_dts <- "pred" 
  
}


# ---------------------------------------- load data 


foi_data <- read.csv(
  file.path("output", "foi", "All_FOI_estimates_linear_env_var.csv"),
  header = TRUE)

adm_preds <- readRDS(  
  file.path("output",
            "predictions",
            model_type,
            "adm_1_predictions.rds"))

square_preds <- readRDS(
  file.path("output",
            "predictions",
            model_type,
            "square_predictions.rds"))

pxl_preds <- readRDS(
  file.path("output",
            "predictions",
            model_type,
            "pred_0_0083_deg_long.rds"))
  
full_square_dts <- readRDS(
  file.path("output", 
            "EM_algorithm", 
            "env_variables", 
            "aggreg_pixel_level_env_vars_20km.rds"))  


# ---------------------------------------- pre process the data


# remove pseudo absences 
foi_data <- subset(foi_data, type != "pseudoAbsence")

# rename FOI field
names(foi_data)[names(foi_data) == "FOI"] <- "o_j"


# ---------------------------------------- pre process admin predictions


adm_df <- adm_preds[!duplicated(adm_preds[, c("ID_0", "ID_1")]), ]

names(adm_df)[names(adm_df) == "mean_pred"] <- "adm_pred"#, "adm_sd", "adm_low_perc", "adm_up_perc")


# ---------------------------------------- pre process square predictions 


if (mod_tp == "boot"){
  
  square_preds_train <- cbind(full_square_dts, square_preds[, 1:3])

  square_preds_test <- cbind(full_square_dts, square_preds[, 4:6])

} else {
  
  square_preds <- cbind(full_square_dts, square_preds)

}


# ---------------------------------------- average up 


average_pxl <- average_up(
  pxl_df = pxl_preds, 
  grp_flds = c("data_id", "ADM_0", "ADM_1"), 
  var_names = var_names_pxl_dts)

names(average_pxl)[names(average_pxl) == "ADM_0"] <- "ID_0"
names(average_pxl)[names(average_pxl) == "ADM_1"] <- "ID_1" 
names(average_pxl)[names(average_pxl) == "mean_pred"] <- "mean_pxl_pred" 
# "mean_pxl_sd", "mean_pxl_lp", "mean_pxl_up"

if (mod_tp == "boot") {
  
  average_sqr_train <- average_up(
    pxl_df = square_preds_train,
    grp_flds = c("data_id", "ADM_0", "ADM_1"),
    var_names = var_names_sqr_dts_train)
  
  names(average_sqr_train)[names(average_sqr_train) == "ADM_0"] <- "ID_0"
  names(average_sqr_train)[names(average_sqr_train) == "ADM_1"] <- "ID_1" 
  
  average_sqr_test <- average_up(
    pxl_df = square_preds_test,
    grp_flds = c("data_id", "ADM_0", "ADM_1"),
    var_names = var_names_sqr_dts_test)
  
  names(average_sqr_test)[names(average_sqr_test) == "ADM_0"] <- "ID_0"
  names(average_sqr_test)[names(average_sqr_test) == "ADM_1"] <- "ID_1" 
  
} else {
  
  average_sqr <- average_up(
    pxl_df = square_preds,
    grp_flds = c("data_id", "ADM_0", "ADM_1"),
    var_names = var_names_sqr_dts)
  
  names(average_sqr)[names(average_sqr) == "ADM_0"] <- "ID_0"
  names(average_sqr)[names(average_sqr) == "ADM_1"] <- "ID_1" 
  names(average_sqr)[names(average_sqr) == var_names_sqr_dts] <- "mean_square_pred" 

}
  

# ---------------------------------------- join 


m_1 <- merge(
  foi_data[, c("data_id", "ID_0", "ID_1", "o_j")],
  adm_df[, c("ID_0", "ID_1", "adm_pred")],
  by = c("ID_0", "ID_1"),
  all.y = FALSE)

m_2 <- merge(
  m_1,
  average_pxl[, c("data_id", "ID_0", "ID_1", "mean_pxl_pred")],
  by = c("data_id", "ID_0", "ID_1"),
  all.y = FALSE)

if (mod_tp == "boot") {
  
  m_3 <- merge(
    m_2,
    average_sqr_train[, c("data_id", "ID_0", "ID_1", "mean_train")],
    by = c("data_id", "ID_0", "ID_1"),
    all.y = FALSE)
  
  m_final <- merge(
    m_3,
    average_sqr_test[, c("data_id", "ID_0", "ID_1", "mean_test")],
    by = c("data_id", "ID_0", "ID_1"),
    all.y = FALSE)
  
} else {

  m_final <- merge(
    m_2,
    average_sqr[, c("data_id", "ID_0", "ID_1", "mean_square_pred")],
    by = c("data_id", "ID_0", "ID_1"),
    all.y = FALSE)
    
}

write_out_csv(m_final, out_path, out_name)


# ---------------------------------------- write out dataframe


# write_out_csv(
#   dat = FOImerg_dat,
#   my_path = output_path,
#   file_name = )

# ---------------------------------------- join datasets 


# finalData <- merge(
#   adm_df, 
#   aggreg_pred, 
#   by = merged_flds, 
#   all = FALSE)
# 
# FOImerg_dat <- merge(
#   finalData, 
#   aggreg_foi, 
#   by = merged_flds, 
#   all.x = FALSE,
#   all.y = FALSE)

# FOImerg_dat <- melt(
#   FOImerg_dat, 
#   id.vars = c("ID_0", "ID_1", "FOI"),
#   measure.vars = c("adm_pred", "mean_pxl_pred"),
#   variable.name = "pred_scale")

# generic_scatter_plot(
#   df_to_plot = FOImerg_dat,
#   x = "FOI",
#   y = "value",
#   my_path = output_figure_path, 
#   file_name = "predictions_vs_data_by_scale.jpg", 
#   x_axis_tag = "data", 
#   y_axis_tag = "predictions", 
#   ttl = NULL, 
#   alpha = NULL, 
#   reverse_x_axis = FALSE,
#   mirror = TRUE,
#   fit_line = TRUE,
#   facet = TRUE, 
#   facet_var = "pred_scale")
