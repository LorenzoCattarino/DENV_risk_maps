# Extract partial dependence information and 
# make partial dependence plots

options(didehpc.cluster = "fi--didemrchnb")

my_resources <- c(
  file.path("R", "utility_functions.r"),
  file.path("R", "random_forest", "functions_for_partial_dependence_plots.R"))

my_pkgs <- c("ggplot2")

context::context_log_start()
ctx <- context::context_save(path = "context",
                             sources = my_resources,
                             packages = my_pkgs)

context::context_load(ctx)
#context::parallel_cluster_start(8, ctx)


# define parameters ----------------------------------------------------------- 


model_type <- "R0_3_boot_model"

RF_mod_name <- "RF_obj_sample"

no_fits <- 200

pdp_pt <- file.path("output",
                    "EM_algorithm",
                    model_type,
                    "partial_dependence")

v_imp_pt <- file.path("output",
                      "EM_algorithm",
                      model_type,
                      "variable_importance")

out_pt <- file.path("figures",
                    "EM_algorithm",
                    model_type)
  
  
# load data -------------------------------------------------------------------


predictor_rank <- read.csv(
  file.path("output", 
            "variable_selection", 
            "metropolis_hastings", 
            "exp_1", 
            "variable_rank_final_fits_exp_1.csv"),
  stringsAsFactors = FALSE)


# pre processing -------------------------------------------------------------- 


variables <- predictor_rank$variable[1:9]

pd_table_fls <- list.files(pdp_pt, 
                           pattern = ".",
                           full.names = TRUE)

vi_table_fls <- list.files(v_imp_pt,
                           pattern = ".",
                           full.names = TRUE) 

pd_tables <- loop(
  pd_table_fls, 
  readRDS,
  parallel = FALSE)

vi_tables <- loop(
  vi_table_fls, 
  readRDS,
  parallel = FALSE)


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
    width = 9,
    height = 7,
    units = "in",
    pointsize = 12,
    res = 300)

p <- ggplot(final_pd_df, aes(x, q50)) +
  facet_wrap(facets = ~ var, 
             ncol = 3,
             scales = "free", 
             labeller = as_labeller(x_name_strips)) +
  geom_ribbon(data = final_pd_df, 
              mapping = aes(ymin = q05, ymax = q95), 
              fill = "gray70", 
              alpha = 0.5) +
  geom_line() +
  theme_bw(base_size = 11, base_family = "") +
  theme(plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "in"))+
  labs(x = "Value of predictor",
       y = "Response (and 90% CI)",
       title = NULL)

print(p)

dev.off()
