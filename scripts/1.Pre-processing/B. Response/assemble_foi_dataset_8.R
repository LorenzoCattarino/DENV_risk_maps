# Attach country birth rates to each data point and calculalte new response variable

library(dplyr)
library(ggplot2)

source(file.path("R", "utility_functions.R"))


# load data ------------------------------------------------------------------- 


foi_data <- read.csv(file.path("output", 
                               "foi", 
                               "All_FOI_estimates_and_predictors.csv"),
                     stringsAsFactors = FALSE) 

birth_rates <- read.csv(file.path("output",
                                    "datasets",
                                    "country_age_structure.csv"),
                          stringsAsFactors = FALSE)


# process ---------------------------------------------------------------------


foi_data_2 <- left_join(foi_data, birth_rates[, c("ID_0", "birth_rate")])

my_plot <- ggplot(subset(foi_data_2, type != "pseudoAbsence")) +
  geom_point(aes(FOI, birth_rate, colour = type), size = 1)

save_plot(my_plot, 
          file.path("figures", "data"),
          "FOI_vs_birth_rate",
          12,
          8)

foi_data_2$Z <- 35 * (foi_data_2$FOI / foi_data_2$mean_age)

write_out_csv(foi_data_2, 
              file.path("output", "foi"), 
              "All_FOI_estimates_and_predictors.csv")
