# extract partial dependence information and 
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
  dependent_variable = "FOI",
  no_predictors = 9)   

year.i <- 2007
year.f <- 2014
ppyear <- 64

model_type_tag <- "_best_model_6"

extra_predictors <- "log_pop_den"


# define variables ------------------------------------------------------------


model_type <- paste0(parameters$dependent_variable, model_type_tag)

pdp_pt <- file.path("output",
                    "EM_algorithm",
                    "best_fit_models",
                    model_type,
                    "partial_dependence")

v_imp_pt <- file.path("output",
                      "EM_algorithm",
                      "best_fit_models",
                      model_type,
                      "variable_importance")

out_pt <- file.path("figures",
                    "EM_algorithm",
                    "best_fit_models",
                    model_type)


# load data -------------------------------------------------------------------


predictor_rank <- read.csv(file.path("output", 
                                     "variable_selection", 
                                     "metropolis_hastings", 
                                     "exp_1", 
                                     "variable_rank_final_fits_exp_1.csv"),
                           stringsAsFactors = FALSE)


# pre processing -------------------------------------------------------------- 


my_predictors <- predictor_rank$name[1:parameters$no_predictors]
my_predictors <- c(my_predictors, extra_predictors)

pd_tables <- readRDS(file.path(pdp_pt, "par_dep.rds"))

vi_table <- readRDS(file.path(v_imp_pt, "var_imp.rds"))


# exctract --------------------------------------------------------------------


final_pd_df_ls <- lapply(pd_tables, edit_pd_list)

final_pd_df <- do.call("rbind", final_pd_df_ls)

final_pd_df$var <- factor(final_pd_df$var, levels = as.character(unique(final_pd_df$var)))
  

# rescale x axes --------------------------------------------------------------


final_pd_df_splt <- split(final_pd_df$x, final_pd_df$var)

for (i in seq_along(final_pd_df_splt)){
  
  one_set <- final_pd_df_splt[i]
  
  var <- names(one_set)
  
  scale <- 1
  
  if(grepl("Re.", var) | grepl("Im.", var)){
    
    scale <- ppyear * (year.f - year.i + 1) / 2 
    
  } 
  
  if(grepl("const_term$", var)){
    
    scale <- ppyear * (year.f - year.i + 1) 
    
  }  
  
  message(scale)
  
  final_pd_df_splt[[i]] <- one_set[[var]] / scale
  
}

final_pd_df$x <- unname(unlist(final_pd_df_splt))


# sort by var importance ------------------------------------------------------


final_pd_df$var <- factor(final_pd_df$var, levels = as.character(vi_table$variable))


# plot ------------------------------------------------------------------------


# create new name strips for facet plots
new_names <- sprintf("%s (%s)", 
                     vi_table$variable, 
                     paste0(round(vi_table$percentage * 100, 2),"%"))

x_name_strips <- setNames(new_names, vi_table$variable)

dir.create(out_pt, FALSE, TRUE)

png(file.path(out_pt, "partial_dependence_plots.png"),
    width = 16.5,
    height = 15,
    units = "cm",
    pointsize = 12,
    res = 300)

p <- ggplot(final_pd_df, aes(x, mean_response)) +
  facet_wrap(facets = ~ var, 
             ncol = 3,
             scales = "free", 
             labeller = as_labeller(x_name_strips)) +
  geom_ribbon(data = final_pd_df, 
              mapping = aes(ymin = mean_response - stddev_response, ymax = mean_response + stddev_response), 
              fill = "gray80", 
              alpha = 0.5) +
  geom_line() +
  theme_bw(base_size = 11, base_family = "") +
  theme(plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "cm"))+
  labs(x = "Value of predictor",
       y = "Response (and SD)",
       title = NULL) +
  theme(strip.text.x = element_text(size = 8))#,
#axis.text.x = element_text(size = 8))

print(p)

dev.off()
