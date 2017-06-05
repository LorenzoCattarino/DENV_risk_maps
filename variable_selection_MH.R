rm(list = ls())

my_resources <- c(
  file.path("R", "convert_df_to_list.R"),
  file.path("R", "random_forest", "wrapper_to_multi_factor_MH_var_sel.R"),
  file.path("R", "random_forest", "grid_up_foi_dataset.R"),
  file.path("R", "random_forest", "bootstrap_foi_dataset.R"),
  file.path("R", "random_forest", "fit_random_forest_model.R"),
  file.path("R", "random_forest", "get_1_0_point_position.R"),
  file.path("R", "random_forest", "calculate_sum_squared_errors.R"),
  file.path("R", "random_forest", "spatial_sampK_cv_rng3.R"),
  file.path("R", "random_forest", "plot_MH_var_sel_outputs.R"),
  file.path("R", "random_forest", "write_out_rds.R"))

my_pkgs <- c("ranger", "weights", "gridExtra")

CLUSTER <- TRUE

analysis_tag <- "variable_selection_MH"

exp_id <- 1


# ------------------------------------- Are you using the cluster?


if(CLUSTER) {
  
  # install.packages("didewin",
  #                  repos = c(CRAN = "https://cran.rstudio.com",
  #                            drat = "https://richfitz.github.io/drat"))
  
  # Load packages
  library(context)
  library(queuer)
  library(didewin)
  
  my_workdir <- "Q:/dengue_risk_mapping"
  
  didewin::didewin_config_global(cluster = "fi--didemrchnb", workdir = my_workdir)
  
  root <- file.path(my_workdir, "context")
  
  ctx <- context::context_save(packages = my_pkgs,
                               sources = my_resources,
                               root = root)
  
  obj <- didewin::queue_didewin(ctx, sync = "R")
  
}else{
  
  # Load packages
  sapply(my_pkgs, library, character.only = TRUE)
  
  # Load functions 
  sapply(my_resources, source)
  
}


# ---------------------------------------- Load data


# load dataset for RF training/validating (model dataset)
dengue_dataset <- read.csv(file.path("data", "foi", "All_FOI_estimates_linear_env_var.csv")) 

# predicting variable names
all_predictors <- read.table(file.path("output", "datasets", "all_predictors.txt"), 
                             header = TRUE, 
                             stringsAsFactors = FALSE)


# ---------------------------------------- Pre-processing the datasets


# remove NA and outliers from model dataset
dengue_dataset <- dengue_dataset[!is.na(dengue_dataset$FOI), ]
dengue_dataset <- subset(dengue_dataset, country != "French Polynesia" & country != "Haiti")

# sort data in decreasing FOI value
dengue_dataset <- dengue_dataset[order(dengue_dataset$FOI, decreasing = TRUE), ]

# assign case weights 
dengue_dataset$new.weight <- 1
dengue_dataset[dengue_dataset$type == "pseudoAbsence", "new.weight"] <- 0.25

# add an ID for each data point
dengue_dataset <- cbind(id = seq_len(nrow(dengue_dataset)), dengue_dataset)

# get all predictor names except land use classes
all_predictors_no_LC <- all_predictors$variable[1:26]


# ---------------------------------------- Set up simulation framework


bootstrap_runs <- 200

factor_combinations <- expand.grid(run_ID = seq_len(bootstrap_runs),
                                   exp_ID = exp_id, 
                                   scaling_factor = 10000)

factor_combinations_list <- df_to_list(factor_combinations, use_names = TRUE)

nt <- 500
ns <- 20
gs <- 5
pa <- 0
it <- 100000
ctof <- it * 0.3
top_it <- 10


# ---------------------------------------- Run Metropolis Hastings algorithm


# # try one job
# t <- obj$enqueue(
#   wrapper_to_multi_factor_MH_var_sel(
#     factor_combinations_list[[1]],
#     model_dataset = dengue_dataset,
#     predictors = all_predictors_no_LC,
#     dependent_variable = "FOI",
#     no_trees = nt,
#     min_node_size = ns,
#     grid_size = gs,
#     pseudoAbs_value = pa,
#     Niter = it,
#     var_scale = 0)

if (CLUSTER) {
  
  task_b_name <- paste(analysis_tag, exp_id, sep = "_exp_")
  
  chains <- queuer::qlapply(
    factor_combinations_list, 
    wrapper_to_multi_factor_MH_var_sel,
    obj,
    model_dataset = dengue_dataset, 
    predictors = all_predictors_no_LC,
    dependent_variable = "FOI", 
    no_trees = nt,
    min_node_size = ns,
    grid_size = gs,
    pseudoAbs_value = pa,
    Niter = it, 
    var_scale = 0,
    timeout = 0,
    name = task_b_name)
  
} else {
  
  chains <- lapply(
    factor_combinations_list[1],
    wrapper_to_multi_factor_MH_var_sel,
    model_dataset = dengue_dataset, 
    predictors = all_predictors_no_LC,
    dependent_variable = "FOI", 
    no_trees = nt,
    min_node_size = ns,
    grid_size = gs,
    pseudoAbs_value = pa,
    Niter = it, 
    var_scale = 0)
  
}
