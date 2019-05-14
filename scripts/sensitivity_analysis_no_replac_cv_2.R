exp_id <- 7

tuned_parameter <- "predictions"

results <- t(sapply(RF_run, "[[", 1))

output <- cbind(factor_combinations, results)

file_name <- paste0("summary_table", "_exp_", exp_id, ".csv")

write.table(output, file.path("output", 
                              "dengue_dataset", 
                              tuned_parameter, 
                              paste("exp", exp_id, sep = "_"),
                              file_name), 
            sep = ",",
            row.names= FALSE)


# ----------------------------------------

# plot individual runs
# first choose the run for which to plot predictions vs observations

# ----------------------------------------


my_run_ids <- 5:6

for (i in seq_along(my_run_ids)){
  
  a <- my_run_ids[i]
  
  x <- RF_run[[a]]
  
  # rotate df from wide to long to allow faceting
  obs_preds_df_long <- melt(
    x[[2]], 
    id.vars = c("country_code", "adm1", "y.data"),
    variable.name = "dataset")
  
  # plot
  RF.preds.vs.obs.plot.stratif.no.labels(
    run_id = a,
    exp_id = exp_id,
    diagnostics = x[[1]],
    predictions = obs_preds_df_long,
    my_path = file.path("figures", 
                        "dengue_dataset", 
                        tuned_parameter,
                        paste("exp", exp_id, sep = "_")))

}
