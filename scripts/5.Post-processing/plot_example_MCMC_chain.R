rm(list = ls())
   
# load packages 
library(ggplot2)

# define variables 
exp_id <- 1
run_id <- 8

# load data
my_chain <- readRDS(
  file.path("output", 
            "dengue_dataset", 
            "variable_selection", 
            "metropolis_hastings", 
            "exp_1", 
            paste0("chain_run_", run_id, ".rds")))


my_chain_df <- as.data.frame(my_chain)

# plot and save
ggplot(
  my_chain_df, aes(iter, cur.OF_after)) +
  geom_line() +
  scale_x_continuous("Iterations") +
  scale_y_continuous("Objective Function") +
  theme(axis.title.x = element_text(size = 15, margin = margin(t = 15)),
        axis.title.y = element_text(size = 15, margin = margin(r = 15)),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15))

file_tag <- paste0("example_MCMC_chain", paste("run", run_id, sep = "_"), ".tiff")

ggsave(file.path("figures", 
                 "dengue_dataset", 
                 "variable_selection", 
                 "metropolis_hastings", 
                 paste("exp", exp_id, sep = "_"),
                 file_tag), 
       dpi = 300)

