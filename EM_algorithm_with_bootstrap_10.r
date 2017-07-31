library(reshape2)
library(ggplot2)
library(plyr)

source(file.path("R", "random_forest", "RF_preds_vs_obs_stratified_plot.r"))
source(file.path("R", "get_lm_equation.r"))

no_fits <- 200

model_type <- "boot_model_20km_cw"

in_path <- file.path(
  "output",
  "predictions",
  model_type,
  "all_scale_predictions",
  "boot_samples") 

out_path <- file.path(
  "figures",
  "EM_algorithm",
  model_type,
  "scatter_plots",
  "boot_samples")
  
for (i in seq_len(no_fits)) {
  
  dts_nm <- paste0("all_scale_predictions_", i, ".rds")
  
  dts <- readRDS(file.path(in_path, dts_nm))
    
  names(dts)[names(dts) == "train"] <- "dataset"
  
  dts$dataset <- factor(x = dts$dataset, levels = c(1,0), labels = c("train", "test"))
  
  # rotate df from wide to long to allow faceting
  dts_mlt <- melt(
    dts, 
    id.vars = c("data_id", "ADM_0", "ADM_1", "o_j", "dataset"),
    measure.vars = c("adm_pred", "mean_square_pred", "mean_pxl_pred"),
    variable.name = "scale")
  
  fl_nm <- paste0("pred_vs_obs_plot_sample_", i,".jpg")
  
  RF_preds_vs_obs_plot_stratif(
    x = "o_j",
    y = "value",
    facet_var = "scale",
    file_name = fl_nm,
    predictions = dts_mlt,
    my_path = out_path)

}
