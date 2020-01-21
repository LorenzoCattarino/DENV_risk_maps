

# Create Excel workbook with data of all plots for STM

library(xlsx)


# define parameters -----------------------------------------------------------


cols_to_keep <- c("phi_set_id", "treatment", "mean", "lCI", "uCI")

cols_to_keep_2 <- c("o_j", "mean_p_i")
  
wol_treatment_levels <- c(0.7, 0.3)

vac_treatment_levels <- c(9, 16, 0)


# define variables ------------------------------------------------------------


out_path <- file.path("output",
                      "predictions_world", 
                      "bootstrap_models")

out_nm <- "Data_file_S1.xlsx" 


# start -----------------------------------------------------------------------


fig_3_A <- read.csv(file.path("output", 
                              "predictions_world", 
                              "bootstrap_models",
                              "prop_change_cases_wolbachia.csv"),
                    stringsAsFactors = FALSE)

fig_3_A <- fig_3_A[, cols_to_keep]

fig_3_A$sub_plot <- "A"

fig_3_B <- read.csv(file.path("output", 
                              "predictions_world", 
                              "bootstrap_models",
                              "total_cases_wolbachia.csv"),
                    stringsAsFactors = FALSE)

fig_3_B <- fig_3_B[, cols_to_keep]

fig_3_B$sub_plot <- "B"

fig_3_C <- read.csv(file.path("output", 
                              "predictions_world", 
                              "bootstrap_models",
                              "dengue_free_countries_wolbachia.csv"),
                    stringsAsFactors = FALSE)

fig_3_C <- fig_3_C[, cols_to_keep]

fig_3_C$sub_plot <- "C"

fig_3 <- rbind(fig_3_A, fig_3_B, fig_3_C)


fig_5 <- read.csv(file.path("output", 
                              "predictions_world", 
                              "bootstrap_models",
                              "prop_change_cases_vaccine.csv"),
                    stringsAsFactors = FALSE)

fig_5 <- fig_5[, cols_to_keep]

fig_S9_A <- read.csv(file.path("output",
                                "datasets",
                                "corr_plots_psAbs_A.csv"),
                      stringsAsFactors = FALSE)
fig_S9_A <- fig_S9_A[, cols_to_keep_2]
fig_S9_A$sub_plot <- "A"

fig_S9_B <- read.csv(file.path("output",
                                "datasets",
                                "corr_plots_psAbs_B.csv"),
                      stringsAsFactors = FALSE)
fig_S9_B <- fig_S9_B[, cols_to_keep_2]
fig_S9_B$sub_plot <- "B"

fig_S9_C <- read.csv(file.path("output",
                                "datasets",
                                "corr_plots_psAbs_C.csv"),
                      stringsAsFactors = FALSE)
fig_S9_C <- fig_S9_C[, cols_to_keep_2]
fig_S9_C$sub_plot <- "C"

fig_S9_D <- read.csv(file.path("output",
                                "datasets",
                                "corr_plots_psAbs_D.csv"),
                      stringsAsFactors = FALSE)
fig_S9_D <- fig_S9_D[, cols_to_keep_2]
fig_S9_D$sub_plot <- "D"

fig_S9_E <- read.csv(file.path("output",
                                "datasets",
                                "corr_plots_psAbs_E.csv"),
                      stringsAsFactors = FALSE)
fig_S9_E <- fig_S9_E[, cols_to_keep_2]
fig_S9_E$sub_plot <- "E"

fig_S9 <- rbind(fig_S9_A, fig_S9_B, fig_S9_C, fig_S9_D, fig_S9_E)

fig_S10_A <- read.csv(file.path("output",
                                "datasets",
                                "corr_plots_no_psAbs_A.csv"),
                      stringsAsFactors = FALSE)
fig_S10_A <- fig_S10_A[, cols_to_keep_2]
fig_S10_A$sub_plot <- "A"

fig_S10_B <- read.csv(file.path("output",
                                "datasets",
                                "corr_plots_no_psAbs_B.csv"),
                      stringsAsFactors = FALSE)
fig_S10_B <- fig_S10_B[, cols_to_keep_2]
fig_S10_B$sub_plot <- "B"

fig_S10_C <- read.csv(file.path("output",
                                "datasets",
                                "corr_plots_no_psAbs_C.csv"),
                      stringsAsFactors = FALSE)
fig_S10_C <- fig_S10_C[, cols_to_keep_2]
fig_S10_C$sub_plot <- "C"

fig_S10_D <- read.csv(file.path("output",
                                "datasets",
                                "corr_plots_no_psAbs_D.csv"),
                      stringsAsFactors = FALSE)
fig_S10_D <- fig_S10_D[, cols_to_keep_2]
fig_S10_D$sub_plot <- "D"

fig_S10_E <- read.csv(file.path("output",
                                "datasets",
                                "corr_plots_no_psAbs_E.csv"),
                      stringsAsFactors = FALSE)
fig_S10_E <- fig_S10_E[, cols_to_keep_2]
fig_S10_E$sub_plot <- "E"

fig_S10 <- rbind(fig_S10_A, fig_S10_B, fig_S10_C, fig_S10_D, fig_S10_E)

fig_S15 <- read.csv(file.path("output",
                              "EM_algorithm",
                              "bootstrap_models",
                              "model_2",
                              "partial_dependence",
                              "data_of_the_pd_plots.csv"),
                    stringsAsFactors = FALSE)

fig_S16 <- read.csv(file.path("output",
                              "EM_algorithm",
                              "bootstrap_models",
                              "model_4",
                              "partial_dependence",
                              "data_of_the_pd_plots.csv"),
                    stringsAsFactors = FALSE)

fig_S19 <- read.csv(file.path("output", 
                              "predictions_world", 
                              "bootstrap_models",
                              "prop_change_infections_wolbachia.csv"),
                    stringsAsFactors = FALSE)

fig_S19 <- fig_S19[, cols_to_keep]

fig_S19 <- fig_S19[fig_S19$treatment %in% wol_treatment_levels, ]

fig_S20 <- read.csv(file.path("output", 
                              "predictions_world", 
                              "bootstrap_models",
                              "prop_change_cases_wolbachia.csv"),
                    stringsAsFactors = FALSE)

fig_S20 <- fig_S20[, cols_to_keep]

fig_S20 <- fig_S20[fig_S20$treatment %in% wol_treatment_levels, ]

fig_S21 <- read.csv(file.path("output", 
                              "predictions_world", 
                              "bootstrap_models",
                              "prop_change_hosp_wolbachia.csv"),
                    stringsAsFactors = FALSE)

fig_S21 <- fig_S21[, cols_to_keep]

fig_S21 <- fig_S21[fig_S21$treatment %in% wol_treatment_levels, ]

fig_S22 <- read.csv(file.path("output", 
                              "predictions_world", 
                              "bootstrap_models",
                              "prop_change_infections_vaccine.csv"),
                    stringsAsFactors = FALSE)

fig_S22 <- fig_S22[, cols_to_keep]

fig_S22 <- fig_S22[fig_S22$treatment %in% vac_treatment_levels, ]

fig_S23 <- read.csv(file.path("output", 
                              "predictions_world", 
                              "bootstrap_models",
                              "prop_change_cases_vaccine.csv"),
                    stringsAsFactors = FALSE)

fig_S23 <- fig_S23[, cols_to_keep]

fig_S23 <- fig_S23[fig_S23$treatment %in% vac_treatment_levels, ]

fig_S24 <- read.csv(file.path("output", 
                              "predictions_world", 
                              "bootstrap_models",
                              "prop_change_hosp_vaccine.csv"),
                    stringsAsFactors = FALSE)

fig_S24 <- fig_S24[, cols_to_keep]

fig_S24 <- fig_S24[fig_S24$treatment %in% vac_treatment_levels, ]

fig_S27 <- read.csv(file.path("output",
                             "sensitivity_analysis",
                             "grid_size",
                             "exp_1",
                             "result_summary_exp_1.csv"),
                   stringsAsFactors = FALSE)

fig_S27 <- fig_S27[, c("grid_size", "no_cells_valid")]

fig_S28 <- read.csv(file.path("output", "datasets", "pseudo_absences_weights.csv"))

fig_S29 <- read.csv(file.path("output",
                              "sensitivity_analysis",
                              "tree_number",
                              "exp_1",
                              "result_summary_exp_1.csv"),
                    stringsAsFactors = FALSE)

fig_S29 <- fig_S29[, c("tree_num", "SS_valid")]

fig_S30 <- read.csv(file.path("output",
                              "sensitivity_analysis",
                              "node_size",
                              "exp_1",
                              "result_summary_exp_1.csv"),
                    stringsAsFactors = FALSE)

fig_S30 <- fig_S30[, c("node_size", "SS_valid")]

fig_S31 <- read.csv(file.path("output",
                              "sensitivity_analysis",
                              "pseudo_absence_value",
                              "exp_1",
                              "result_summary_exp_1.csv"),
                    stringsAsFactors = FALSE)

fig_S31 <- fig_S31[, c("pseudo_absence_value", "SS_valid")]

fig_S32 <- read.csv(file.path("output",
                              "variable_selection",
                              "stepwise_v6",
                              "sample_1",
                              "addition", 
                              "output_from_addition.csv"),
                    stringsAsFactors = FALSE)

fig_S33 <- read.csv(file.path("output",
                              "variable_selection",
                              "stepwise_v6",
                              "freq_distr_no_top_predictors.csv"),
                    stringsAsFactors = FALSE)

fig_S34 <- read.csv(file.path("output",
                              "EM_algorithm",
                              "best_fit_models",
                              "model_1",
                              "diagnostics",
                              "diagnostic_data_plot.csv"),
                    stringsAsFactors = FALSE)


# combine all -----------------------------------------------------------------


all_figs_data <- list(fig_3, fig_5,
                      fig_S9, fig_S10, fig_S15, fig_S16,
                      fig_S19, fig_S20, fig_S21, fig_S22, fig_S23, fig_S24, 
                      fig_S27, fig_S28, fig_S29, fig_S30, fig_S31, fig_S32, fig_S33, fig_S34)
  
fig_nums_mainBody <- c(3, 5) 

fig_nums_SM <- c(seq(9, 18, 1), 23, 24, seq(27, 32, 1)) 

sheet_names <- c(paste0("fig_", fig_nums_mainBody),  
                paste0("fig_S", fig_nums_SM))

for (i in seq_along(sheet_names)){
  
  sn <- sheet_names[i]
  
  message(sn)
  
  write.xlsx(all_figs_data[i], 
             file = file.path(out_path, out_nm),
             sheetName = sn, 
             row.names = FALSE,
             append = TRUE)
}
