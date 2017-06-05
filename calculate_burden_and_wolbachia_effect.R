options(didewin.cluster = "fi--didemrchnb")

CLUSTER <- FALSE

my_resources <- c(
  file.path("R", "utility_functions.R"),
  file.path("R", "burden_and_interventions", "get_age_band_bounds.R"),
  file.path("R", "burden_and_interventions", "wrapper_for_running_multi_factor_burden.R"),  
  file.path("R", "burden_and_interventions", "wrapper_for_getting_multi_foi_burden.R"),
  file.path("R", "burden_and_interventions", "calculate_infection_probability_and_number.R"),
  file.path("R", "burden_and_interventions", "calculate_R0.R"),
  file.path("R", "burden_and_interventions", "calculate_total_incidence.R"),
  file.path("R", "burden_and_interventions", "calculate_incidence_of_infections.R"),
  file.path("R", "burden_and_interventions", "calculate_average_infect_probab.R"),
  file.path("R", "burden_and_interventions", "calculate_probabilities_and_R0.R"))

my_pkgs <- c("approximate")

context::context_log_start()
ctx <- context::context_save(packages = my_pkgs,
                             sources = my_resources,
                             root = "context",
                             package_sources = context::package_sources(github = "richfitz/approximate"))


# ---------------------------------------- are you using the cluster?


if (CLUSTER) {
  
  #config <- didewin::didewin_config(template = "12and16Core")
  obj <- didewin::queue_didewin(ctx)

} else {
  
  context::context_load(ctx)
  #context::start_parallel_cluster(8, ctx)

}


# ---------------------------------------- define parameters


exp_ID <- 1

adm_levels <- c(1, 2)

m_flds <- c("ID_0", "ID_1") 
  
pred_in_path <- file.path(
  "output", 
  "dengue_dataset", 
  "predictions", 
  "admin_level_1")
          
out_pth <- file.path(
  "output", 
  "dengue_dataset", 
  "burden", 
  "admin_level_1",
  paste("exp", exp_ID, sep = "_"))

out_fl_nm <- paste0("factor_combinations_exp_", exp_ID, ".csv")


# ---------------------------------------- load data


# load predictions
prediction_datasets <- lapply(adm_levels, function(x){
  readRDS(file.path(pred_in_path, paste0("predictions_adm_", x, ".RDS")))})

country_age_struc <- read.csv(file.path("output", 
                                        "datasets",
                                        "country_age_structure.csv"), 
                              header = TRUE) 
  
  
# ---------------------------------------- extract info from age structure 


# Get names of age band columns
age_band_tgs <- grep("band", names(country_age_struc), value = TRUE)

# Get age band bounds
age_band_bnds <- get_age_band_bounds(age_band_tgs)

age_band_L_bounds <- age_band_bnds[, 1]

age_band_U_bounds <- age_band_bnds[, 2] + 1


# ---------------------------------------- pre process age structure


zero_age_str_countries <- apply(country_age_struc[, age_band_tgs], 1, sum) == 0

country_age_struc <- country_age_struc[!zero_age_str_countries, ]


# ---------------------------------------- pre process prediction datasets 


prediction_datasets[[1]]$country <- as.character(prediction_datasets[[1]]$country)
prediction_datasets[[1]]$name1 <- as.character(prediction_datasets[[1]]$name1)

prediction_datasets <- lapply(prediction_datasets, function(x){
  subset(x, mean_pred != 0)})


# ---------------------------------------- filter out data points with NA age structure data


dd <- setNames(data.frame(country_age_struc[, m_flds[1]]), nm = m_flds[1])

prediction_datasets <- lapply(prediction_datasets, function(x){
  merge(
  x, 
  dd, 
  by = m_flds[1], 
  all.y = FALSE)})


# ---------------------------------------- set up simulation framework


# Get all combinations of factors for burden analysis 
factor_combinations <- expand.grid(
  ID.exp = exp_ID, 
  adm = 1,
  phi = c(1, 2, 3, 4),
  scaling_factor = c(1, 0.3))

# Define weights for symptomaticity of primary, secondary and tertiary infections
sympt_weights <- c(0.45, 0.85, 0.15)

# Rearrange order of varying factors
factor_combinations <- factor_combinations[order(factor_combinations$adm, 
                                                 factor_combinations$phi
                                                 -factor_combinations$scaling_factor), ]

# Add run ID field
factor_combinations <- cbind(ID.run = seq_len(nrow(factor_combinations)), factor_combinations)

# Convert df to list
factor_combinations_list <- df_to_list (x = factor_combinations, use_names = TRUE)


# ---------------------------------------- submit jobs


if (CLUSTER) {
  
  burden_run <- queuer::qlapply(
    factor_combinations_list, 
    burden_multi_factor_wrapper, 
    obj,
    list_of_data = prediction_datasets, 
    my_path = out_pth,
    age_band_tags = age_band_tgs,
    age_band_lower_bounds = age_band_L_bounds,
    age_band_upper_bounds = age_band_U_bounds,
    sympt_weights = sympt_weights,
    age_struc_data = country_age_struc,
    timeout = 0)
  
} else {
  
  burden_run <- loop(
    factor_combinations_list,
    burden_multi_factor_wrapper,
    list_of_data = prediction_datasets, 
    my_path = out_pth,
    age_band_tags = age_band_tgs,
    age_band_lower_bounds = age_band_L_bounds,
    age_band_upper_bounds = age_band_U_bounds,
    sympt_weights = sympt_weights,
    age_struc_data = country_age_struc,
    parallel = FALSE)
  
}

if (!CLUSTER) {
  context:::stop_parallel_cluster()
}

# t <- obj$enqueue(
#   burden_multi_factor_wrapper(
#     factor_combinations_list[[1]],
#     list_of_data = prediction_datasets,
#     my_path = out_pth,
#     age_band_tags = age_band_tgs,
#     age_band_lower_bounds = age_band_L_bounds,
#     age_band_upper_bounds = age_band_U_bounds,
#     sympt_weights = sympt_weights,
#     age_struc_data = country_age_struc))

# write out exp des
write_out_csv(dat = factor_combinations, 
              my_path = out_pth, 
              file_name = out_fl_nm)
