library(dplyr)

source(file.path("R", "utility_functions.R"))
source(file.path("R", "prepare_datasets", "average_up.R"))


# ---------------------------------------- define parameters 


res <- 20 

out_name <- paste0("prediction_scale_comparison_best_", res, "km_cw.csv")

var_names_pxl_dts <- "mean_pred" #, "sd_pred", "low_perc", "up_perc")

var_names_sqr_dts <- "p_i" 

tgt_dir_name <- paste0("best_model_", res, "km_cw")

out_path <- file.path(
  "output",
  "predictions",
  tgt_dir_name)


# ---------------------------------------- load data 


foi_data <- read.csv(
  file.path("output", "All_FOI_estimates_linear_env_var.csv"),
  header = TRUE)

adm_preds <- readRDS(  
  file.path("output",
            "predictions",
            tgt_dir_name,
            "adm_1_predictions.RDS"))

square_preds <- readRDS(
  file.path("output",
            "predictions",
            tgt_dir_name,
            paste0("square_predictions_best_model_", res, "km_cw.RDS")))

pxl_preds <- readRDS(
  file.path("output",
            "predictions",
            tgt_dir_name,
            paste0("pxl_predictions_best_model_", res, "km_cw.RDS")))
  
  
# ---------------------------------------- pre process the data


# remove outliers 
foi_data <- subset(foi_data, ISO != "PYF" & ISO != "HTI")

# remove pseudo absences 
foi_data <- subset(foi_data, type != "pseudoAbsence")

ad_adm <- foi_data %>% group_by_(.dots = c("ID_0", "ID_1"))

o_j <- ad_adm %>% summarise(o_j = mean(FOI))


# ---------------------------------------- pre process admin predictions


adm_df <- adm_preds[!duplicated(adm_preds[, c("ID_0", "ID_1")]), ]

names(adm_df)[names(adm_df) == "mean_pred"] <- "adm_pred"#, "adm_sd", "adm_low_perc", "adm_up_perc")


# ---------------------------------------- average up 


average_pxl <- average_up(
  pxl_df = pxl_preds, 
  grp_flds = c("ADM_0", "ADM_1"), 
  var_names = var_names_pxl_dts)

names(average_pxl)[names(average_pxl) == "ADM_0"] <- "ID_0"
names(average_pxl)[names(average_pxl) == "ADM_1"] <- "ID_1" 
names(average_pxl)[names(average_pxl) == "mean_pred"] <- "mean_pxl_pred" 
# "mean_pxl_sd", "mean_pxl_lp", "mean_pxl_up"

average_sqr <- average_up(
  pxl_df = square_preds,
  grp_flds = c("ID_0", "ID_1"),
  var_names = var_names_sqr_dts)

names(average_sqr)[names(average_sqr) == "p_i"] <- "mean_square_pred" 


# ---------------------------------------- join 


m_1 <- merge(
  o_j[, c("ID_0", "ID_1", "o_j")],
  adm_preds[, c("ID_0", "ID_1", "mean_pred")],
  by = c("ID_0", "ID_1"), 
  all.y = FALSE)

m_2 <- merge(
  m_1,
  average_pxl[, c("ID_0", "ID_1", "mean_pxl_pred")],
  by = c("ID_0", "ID_1"), 
  all.y = FALSE)

m_3 <- merge(
  m_2,
  average_sqr,
  by = c("ID_0", "ID_1"),
  all.y = FALSE)
  
write_out_csv(m_3, out_path, out_name)


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
