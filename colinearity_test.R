# Predictor multicolinearity test ----------------------------------

corr_coeff_mat <- cor(dengue_dataset[, all_predictors])

lower_triang_logical <- lower.tri(corr_coeff_mat, diag = FALSE)

corr_coeff_mat[lower_triang_logical] <- NA

#write.table(corr_coeff_mat, file.path("output", "dengue_dataset", "predictor_corr_coeff_matrix.csv"), row.names = TRUE, sep = ",")

colinear_covariate_indices <- which((corr_coeff_mat > 0.7 | corr_coeff_mat < -0.7) & corr_coeff_mat != 1, arr.ind = TRUE)

colinear_covariates <- unique(colnames(corr_coeff_mat[, colinear_covariate_indices[, "col"]]))
