# Extract partial dependence information and 
# make partial dependence plots

options(didehpc.cluster = "fi--didemrchnb")

my_resources <- c(
  file.path("R", "random_forest", "partial_dependence_plots.R"),
  file.path("R", "utility_functions.R"))

my_pkgs <- c("ggplot2")

context::context_log_start()
ctx <- context::context_save(path = "context",
                             sources = my_resources,
                             packages = my_pkgs)

context::context_load(ctx)
#context::parallel_cluster_start(8, ctx)


# define parameters ----------------------------------------------------------- 


parameters <- list(
  dependent_variable = "R0_3",
  grid_size = 5,
  no_samples = 200,
  no_predictors = 9)   


# define variables ------------------------------------------------------------


model_type <- paste0(parameters$dependent_variable, "_boot_model")

my_dir <- paste0("grid_size_", parameters$grid_size)

pdp_pt <- file.path("output",
                    "EM_algorithm",
                    "bootstrap_models",
                    my_dir,
                    model_type,
                    "partial_dependence")

v_imp_pt <- file.path("output",
                      "EM_algorithm",
                      "bootstrap_models",
                      my_dir,
                      model_type,
                      "variable_importance")

out_pt <- file.path("figures",
                    "EM_algorithm",
                    "bootstrap_models",
                    my_dir,
                    model_type)
  
  
# load data -------------------------------------------------------------------


predictor_rank <- read.csv(file.path("output", 
                                     "variable_selection", 
                                     "metropolis_hastings", 
                                     "exp_1", 
                                     "variable_rank_final_fits_exp_1.csv"),
                           stringsAsFactors = FALSE)


# pre processing -------------------------------------------------------------- 


variables <- predictor_rank$name[1:parameters$no_predictors]

pd_table_fls <- list.files(pdp_pt, 
                           pattern = ".",
                           full.names = TRUE)

vi_table_fls <- list.files(v_imp_pt,
                           pattern = ".",
                           full.names = TRUE) 

pd_tables <- loop(pd_table_fls, readRDS, parallel = FALSE)

vi_tables <- loop(vi_table_fls, readRDS, parallel = FALSE)


# exctract --------------------------------------------------------------------


final_pd_df_ls <- lapply(seq_along(variables), extract_pd, variables, pd_tables)

final_pd_df <- do.call("rbind", final_pd_df_ls)
  
all_vi_values <- lapply(seq_along(variables), extract_vi, variables, vi_tables)
  
importance <- vapply(all_vi_values, mean, numeric(1))  
  
vi_df <- data.frame(var = variables, importance = importance)

final_vi_df <- vi_df[order(vi_df$importance, decreasing = TRUE),]

# reorder the levels of `var` factor based on variable percentage importance 
final_pd_df$var <- factor(final_pd_df$var, 
                          levels = as.character(final_vi_df$var))
  

# plot ------------------------------------------------------------------------


# create new name strips for facet plots
new_names <- sprintf("%s (%s)", 
                     final_vi_df$var, 
                     paste0(round(final_vi_df$importance * 100, 2),"%"))
x_name_strips <- setNames(new_names, final_vi_df$var)

dir.create(out_pt, FALSE, TRUE)

png(file.path(out_pt, "partial_dependence_plots.png"),
    width = 16.5,
    height = 15,
    units = "cm",
    pointsize = 12,
    res = 300)

p <- ggplot(final_pd_df, aes(x, q50)) +
  facet_wrap(facets = ~ var, 
             ncol = 3,
             scales = "free", 
             labeller = as_labeller(x_name_strips)) +
  geom_ribbon(data = final_pd_df, 
              mapping = aes(ymin = q05, ymax = q95), 
              fill = "gray80", 
              alpha = 0.5) +
  geom_line() +
  theme_bw(base_size = 11, base_family = "") +
  theme(plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "cm"))+
  labs(x = "Value of predictor",
       y = "Response (and 95% CI)",
       title = NULL) +
  theme(strip.text.x = element_text(size = 8),
        axis.text.x = element_text(size = 6))

print(p)

dev.off()
