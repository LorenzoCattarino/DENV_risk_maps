# Attach mean country age to each data point and calculalte new response variable

library(dplyr)
library(ggplot2)

source(file.path("R", "utility_functions.R"))


# load data ------------------------------------------------------------------- 


foi_data <- read.csv(file.path("output", 
                               "foi", 
                               "All_FOI_estimates_and_predictors.csv"),
                     stringsAsFactors = FALSE) 

mean_age_data <- read.csv(file.path("output",
                                    "datasets",
                                    "country_age_structure_mean.csv"),
                          stringsAsFactors = FALSE)


# process ---------------------------------------------------------------------


foi_data_2 <- left_join(foi_data, mean_age_data[, c("ID_0", "mean_age", "sd_age")])

my_plot <- ggplot(subset(foi_data_2, type != "pseudoAbsence")) +
  geom_point(aes(FOI, mean_age, colour = type), size = 1)

save_plot(my_plot, 
          file.path("figures", "data"),
          "FOI_vs_mean_country_age",
          12,
          8)

foi_data_2$Z <- foi_data_2$FOI / foi_data_2$mean_age 

write_out_csv(foi_data_2, 
              file.path("output", "foi"), 
              "All_FOI_estimates_and_predictors.csv")
