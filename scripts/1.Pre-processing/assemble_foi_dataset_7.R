# Calculalte new response variable

library(dplyr)
library(ggplot2)

source(file.path("R", "utility_functions.R"))


# load data ------------------------------------------------------------------- 


foi_data <- read.csv(file.path("output", 
                               "foi", 
                               "All_FOI_estimates_and_predictors.csv"),
                     stringsAsFactors = FALSE) 


# process ---------------------------------------------------------------------


my_plot <- ggplot(subset(foi_data, type != "pseudoAbsence")) +
  geom_point(aes(FOI, mean_age, colour = type), size = 1)

save_plot(my_plot, 
          file.path("figures", "data"),
          "FOI_vs_mean_country_age",
          12,
          8)

foi_data$Z <- 35 * (foi_data$FOI / foi_data$mean_age)

write_out_csv(foi_data, 
              file.path("output", "foi"), 
              "All_FOI_estimates_and_predictors.csv")
