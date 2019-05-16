###
# Profiling using profvis
###
rm(list=ls())

install.packages("devtools")
install.packages("digest")
devtools::install_github("rstudio/profvis")

# load packages
library(randomForest)
library(plyr)
library(profvis)

# load functions 
source(file.path("R", "spatial_cv_rf6_core.R"))
source(file.path("R", "predictor_importance_test_functions.R"))

# load data
env.data <- read.table(file.path("data", "FTMap_62046.txt"), header = TRUE, sep="\t")
full.data <- read.csv(file.path("data", "malaria_v2.csv"))

# add one predictor to the data (# 163)
env.data$dtMnt <- env.data$dt_new_const-env.data$nt_new_const  
full.data$dtMnt <- full.data$dt_new_const-full.data$nt_new_const

# get column indices of all the potential predictors from "full.data" database 
all.predictors <- c(57,58,163,61,62,63,64,65,68,80,92)
all.predictors <- all.predictors[order(all.predictors, decreasing = FALSE)]
names(all.predictors) <- colnames(full.data[,all.predictors])

# try all potential predictors
#names_all.predictors <- c("LC0", "LC1", "LC2", "LC4", "LC6", "LC7", "LC8", "LC9", "LC10", "LC11", "LC12", "LC13", "LC14", "LC16", "evimax", "evimin", "mirmax", "mirmin", "daysabove", "below15", "above40", "largefluct", "r_new_const", "dt_new_const", "nt_new_const", "mir_new_const", "evi_new_const", "r_c0", "r_tau0", "r_c1", "r_tau1", "r_c2", "r_tau2", "dt_c0", "dt_tau0", "dt_c1", "dt_tau1", "dt_c2", "dt_tau2", "nt_c0", "nt_tau0", "nt_c1", "nt_tau1", "nt_c2", "nt_tau2", "evi_c0", "evi_tau0", "evi_c1", "evi_tau1", "evi_c2", "evi_tau2", "mir_c0", "mir_tau0", "mir_c1", "mir_tau1", "mir_c2", "mir_tau2", "altitude", "hottestmonth", "maxtem", "coldestmonth", "coldestquarter", "tempcoldestquarter", "hottestquarter", "temphottestquarter", "rainwettestquarter", "raindriestquarter", "precipwarmestquarter", "precipcoldestquarter", "precipwarmestmonth", "precipcoldestmonth", "max_rain", "min_rain", "cov_rain", "max_dt", "min_dt", "max_nt", "min_nt", "range_dt", "range_nt", "range_t", "mean_t", "var_dt", "var_nt", "var_t", "cov_dt", "cov_nt", "cov_t", "range_rain", "sd_rain", "ldens20", "dtMnt")
#all.predictors <- which(colnames(full.data) %in% names_all.predictors)
#names(all.predictors) <- names_all.predictors

parameters <- list(
  replicates = 10,
  gridSizes = 5)

# get factor combinations 
factor_combinations <- get.factor.combinations (parameters, 0)

# vector of folder names 
folder_names_vec <- sapply(factor_combinations, function(x) {paste("run", x["ID.run"], sep="_")})

# create folders for data outputs 
sapply(folder_names_vec, function(x) {dir.create(file.path("output", "predictor_importance_test", x), FALSE, TRUE)})

profvis({
pred_importance_test <- lapply(factor_combinations[[1]], 
                               spatial_cv_rf5.stepwise_variable_addition.multi_grid_sizes_wrapper,
                               env.data, full.data, all.predictors)
}, height = "500px")


############ other approach ##############
Rprof("profile.out", line.profiling=TRUE)
eval(parse(file = file.path("R", "spatial_cv_rf6_core.R"), keep.source=TRUE))
Rprof() 
summaryRprof("profile.out", lines = "show")
