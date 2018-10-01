# Calculate the partial depence of the model function on each explanatory variable,
# for each model fit.

options(didehpc.cluster = "fi--didemrchnb")

CLUSTER <- TRUE

my_resources <- c(
  file.path("R", "random_forest", "partial_dependence_plots_pdp.R"),
  file.path("R", "utility_functions.R"))

my_pkgs <- c("ranger", "pdp")

context::context_log_start()
ctx <- context::context_save(path = "context_2",
                             sources = my_resources,
                             packages = my_pkgs)


# define parameters ----------------------------------------------------------- 


wanted_exp_ids <- c(5:8, 13:16, 21:24)


# start up the cluster -------------------------------------------------------- 


if (CLUSTER) {
  
  obj <- didehpc::queue_didehpc(ctx)
  
} else {
  
  context::context_load(ctx)
  
}

# obj$enqueue(install.packages(file.path("R_sources", "h2o_3.18.0.8.tar.gz"), repos=NULL, type="source"))$wait(Inf)
# obj$enqueue(sessionInfo())$wait(Inf)


# load data -------------------------------------------------------------------


bootstrap_experiments <- read.csv(file.path("output", 
                                            "EM_algorithm", 
                                            "bootstrap_models", 
                                            "boostrap_fit_experiments.csv"),
                                  stringsAsFactors = FALSE)

predictor_rank <- read.csv(file.path("output", 
                                     "variable_selection",
                                     "stepwise",
                                     "predictor_rank.csv"),
                           stringsAsFactors = FALSE)


# pre processing --------------------------------------------------------------


bootstrap_experiments_sub <- subset(bootstrap_experiments, 
                                    exp_id %in% wanted_exp_ids)

bootstrap_experiments_sub_ls <- df_to_list(bootstrap_experiments_sub, TRUE)
  
  
# submit all jobs -------------------------------------------------------------


if (CLUSTER) {

  pd_tables <- queuer::qlapply(
    bootstrap_experiments_sub_ls[seq_len(1000)],
    wrapper_over_factor_combs,
    obj,
    predictor_rank = predictor_rank)

} else {

  pd_tables <- lapply(
    bootstrap_experiments_sub_ls[1],
    wrapper_over_factor_combs,
    predictor_rank = predictor_rank)

}
