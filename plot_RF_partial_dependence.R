options(didewin.cluster = "fi--didemrchnb")

CLUSTER <- FALSE

my_resources <- c(
  file.path("R", "utility_functions.R"))

my_pkgs <- c("ranger", "pdp")
                
context::context_log_start()
ctx <- context::context_save(packages = my_pkgs,
                             sources = my_resources,
                             root = "context")


# ---------------------------------------- are you using the cluster?


if(CLUSTER) {
  config <- didewin::didewin_config(template = "12and16Core")
  obj <- didewin::queue_didewin(ctx, config = config)
}else{
  context::context_load(ctx)
  context::start_parallel_cluster(8, ctx)
}


# ---------------------------------------- load data 


# training data ??????
dengue_dataset <- read.csv(
  file.path("output", 
            "dengue_dataset", 
            "All_FOI_estimates_linear_env_var.csv"),
  header = TRUE) 

# model objects
RF_objs <- readRDS(
  file.path(
    "output", 
    "dengue_dataset", 
    "sensitivity_analysis",
    "final_model",
    "list_of_models_exp_1.RDS"))

# predicting variable rank
predictor_rank <- read.csv(
  file.path("output", 
            "dengue_dataset", 
            "variable_selection", 
            "metropolis_hastings", 
            "exp_1", 
            "variable_rank_final_fits_exp_1.csv"),
  stringsAsFactors = FALSE)


# ---------------------------------------- pre processing 


# get the vector of best predictors (from MH variable selection routine)
best_predictors <- predictor_rank$variable[1:9]
          
all_var_list <- vector("list", length = length(best_predictors))


# ---------------------------------------- run 


for (i in seq_along(best_predictors)){
  
  var <- best_predictors[i]
  
  dependence_values_all_models <- loop(
    RF_objs, 
    partial,
    pred.var = var,
    type = "regression",
    plot = FALSE,
    train = dengue_dataset,
    parallel = TRUE)
  
  len <- nrow(dependence_values_all_models[[i]])
  
  all_vars <- vapply(dependence_values_all_models, "[[", numeric(len), var)
  all_ys <- vapply(dependence_values_all_models, "[[", numeric(len), "yhat")
  
  a <- rowMeans(all_vars)
  b <- rowMeans(all_ys)
  c <- apply(all_ys, 1, sd)
  
  df_var <- data.frame(var_v = a, yhat = b, se = c, var = var)
  all_var_list[[i]] <- df_var

}

if (!CLUSTER) {
  context:::stop_parallel_cluster()
}


# ---------------------------------------- save 


data_to_plot <- do.call("rbind", all_var_list)

saveRDS(data_to_plot, "partial_dependence_values.RDS")


# ---------------------------------------- make partial dependence plots


ggplot(data_to_plot, aes(NightTemp_Re0, yhat, ymin = yhat - se/2, ymax = yhat + se/2)) +
  geom_line(lwd = 1) +
  geom_smooth(stat = "identity", colour = "black") +
  scale_x_continuous(var) +
  scale_y_continuous(paste("f(", var, ")")) +
  theme(axis.title.x = element_text(size = 12, margin = margin(t = 15)),
        axis.title.y = element_text(size = 12, margin = margin(r = 15)),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12))

out_path <- file.path(
  "figures",
  "dengue_dataset",
  "sensitivity_analysis",
  "final_model"
)

file_tag <- paste0("partial_dependence_plots", ".jpg")

dir.create(out_path, FALSE, TRUE)

ggsave(file.path(out_path, file_tag), 
       width = 7, height = 5, units = "in",
       dpi = 300)
