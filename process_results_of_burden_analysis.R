rm(list = ls())

my_resources <- c(
  file.path("R", "random_forest", "convert_df_to_list.R"),
  file.path("R", "burden_and_interventions", "wrapper_for_running_multi_factor_burden.R"),
  file.path("R", "burden_and_interventions", "wrapper_for_mapping_burden_measures.R"),
  file.path("R", "burden_and_interventions", "wrapper_for_calculating_cases.R"),
  file.path("R", "burden_and_interventions", "calculate_incidence_and_cases.R"),
  file.path("R", "burden_and_interventions", "calculate_incidence_of_infections.R"),
  file.path("R", "burden_and_interventions", "wrapper_for_calculating_R0.R"),
  file.path("R", "burden_and_interventions", "calculate_R0.R"),
  file.path("R", "burden_and_interventions", "wrapper_for_back_transforming_R0.R"),
  file.path("R", "burden_and_interventions", "map_admin_burden.R"),
  file.path("R", "burden_and_interventions", "get_age_band_bounds.R")
)

my_pkgs <- c("maptools", "dplyr", "colorRamps", "approximate")

build_queue <- TRUE

# Get task ID
task_bundle_id <- "introvertish_earthworm"


# ---------------------------------------- Get the results


if(build_queue)
{
  # Rebuild the queue
  my_workdir <- "Q:/dengue_risk_mapping"
  context::context_log_start()
  didewin::didewin_config_global(cluster = "fi--didemrchnb", workdir = my_workdir)
  root <- file.path(my_workdir, "context")
  ctx <- context::context_save(packages = my_pkgs,
                               sources = my_resources,
                               root = root,
                               package_sources = context::package_sources(github = "richfitz/approximate"))
  obj <- didewin::queue_didewin(ctx, sync = "R")
  
  # Recreate task object 
  burden_run <- obj$task_bundle_get(task_bundle_id)
}  
  
# Get results
burden_run_results <- burden_run$results()

###
####### Is the following correct when rebuilding the queue???
###

library("ggplot2")
library("reshape2")

source(file.path("R", "burden_and_interventions", "summarize_burden_by_country.R"))
source(file.path("R", "burden_and_interventions", "save_exp_design.R"))
source(file.path("R", "burden_and_interventions", "save_summary_by_country.R"))
source(file.path("R", "burden_and_interventions", "calculate_incidence_of_infections.R"))


# ---------------------------------------- Set up simulation framework


# Get all combinations of factors for burden analysis 
factor_combinations <- expand.grid(
  ID.exp = 3, 
  adm = c(1, 2),
  phi = c(1, 2, 3, 4),
  scaling_factor = c(1, 0.5, 0.3))

# Define weights for symptomaticity of primary, secondary and tertiary infections
sympt_weights <- c(0.45, 0.85, 0.15)

# Rearrange order of varying factors
factor_combinations <- factor_combinations[order(factor_combinations$adm, 
                                                 factor_combinations$phi
                                                 -factor_combinations$scaling_factor), ]

# Add run ID field
factor_combinations <- cbind(ID.run = seq_len(nrow(factor_combinations)), factor_combinations)


# ---------------------------------------- Summarize, by country, cases and incidence, for each run


save_exp_design (unique(factor_combinations$ID.exp), 
                  factor_combinations, 
                  "burden")

total_cases_by_country <- lapply(burden_run_results, calculate_total_cases_by_country)
  
incidence_of_total_cases_by_country <- lapply(total_cases_by_country, calculate_incidence_of_total_cases_by_country)
  
summaries_by_country_per_run <- Map(cbind, total_cases_by_country, incidence_of_total_cases_by_country)

# Output the summary table by country, for each run
sapply(seq_len(nrow(factor_combinations)),
       save_summary_by_country,
       xx = factor_combinations, 
       yy = summaries_by_country_per_run, 
       output_folder = "burden")
  

# ---------------------------------------- Summarize, by run, cases and incidence 


total_cases_by_run <- lapply(
  burden_run_results, function(x){ 
    a <- colSums(x[, c("population", "number_of_infections", "number_of_cases")])
    names(a) <- c("total_pop", "total_infections", "total_cases")
    a
}
)

incidence_of_total_cases_by_run <- lapply(total_cases_by_run, function(x){
  xx <- lapply(x[c("total_infections", "total_cases")], calculate_incidence_of_infections, x["total_pop"])
  xx_un <- unlist(xx)
  names(xx_un) <- c("incidence_of_total_infections", "incidence_of_total_cases")
  xx_un
})

summaries_by_run_list <- Map(c, total_cases_by_run, incidence_of_total_cases_by_run)

summaries_by_run <- do.call("rbind", summaries_by_run_list)

# No scientific format 
#summary <- format(summary, scientific = FALSE, digits = 2, trim = TRUE)

summary_table <- cbind(factor_combinations, summaries_by_run)

# Write out the summary table of the experiment
write.table(summary_table, 
            file.path("output", "dengue_dataset", "burden", 
                      paste("exp", unique(factor_combinations$ID.exp), sep = "_"),
                      sprintf("summary_table_%s%s", 
                              paste("exp", unique(factor_combinations$ID.exp), sep = "_"), 
                              ".csv")), 
            row.names = FALSE, sep = ",")


# ---------------------------------------- Plot total numbers summarised by run  


summary_table_2 <- cbind(factor_combinations, 
                         summary_table[, c("total_infections", "total_cases")])

summary_table_2$scaling_factor <- factor(summary_table_2$scaling_factor, levels = c(1, 0.5, 0.3))

summary_table_2$phi <- factor(summary_table_2$phi)

phi_factor_levels <- c("Up to 2", "Up to 3", "Up to 4", "Up to 4 'double'")

levels(summary_table_2$phi) <- phi_factor_levels

# Melt
summary_table_2_long <- melt(
  summary_table_2,
  id = c("ID.run", "ID.exp", "adm", "phi", "scaling_factor"))

# Plot 
ggplot(summary_table_2_long, aes(scaling_factor, value)) + 
  geom_bar(aes(fill = variable), stat = "identity", position = "dodge") + 
  facet_grid(. ~ phi)

ggsave(file.path("figures", "dengue_dataset", "burden", 
                 paste("exp", unique(factor_combinations$ID.exp), sep = "_"), 
                 "facet_barchart_of_total_number.pdf"),
       width = 12, height = 4, units = "in")


# ---------------------------------------- Plot incidences summarised by run 


summary_table_3 <- cbind(factor_combinations, 
                         summary_table[, c("incidence_of_total_infections", "incidence_of_total_cases")])

summary_table_3$scaling_factor <- factor(summary_table_3$scaling_factor, levels = c(1, 0.5, 0.3))

summary_table_3$phi <- factor(summary_table_3$phi)

levels(summary_table_3$phi) <- phi_factor_levels

# Melt
summary_table_3_long <- melt(
  summary_table_3, 
  id = c("ID.run", "ID.exp", "adm", "phi", "scaling_factor"))

# Plot 
ggplot(summary_table_3_long, aes(scaling_factor, value)) + 
  geom_bar(aes(fill = variable), stat = "identity", position = "dodge") + 
  facet_grid(. ~ phi)

ggsave(file.path("figures", "dengue_dataset", "burden", 
                 paste("exp", unique(factor_combinations$ID.exp), sep = "_"), 
                 "facet_barchart_of_incidence.pdf"),
       width = 12, height = 4, units = "in")
