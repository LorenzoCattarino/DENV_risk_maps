
library(dplyr)


# define parameters -----------------------------------------------------------


model_tp <- "FOI_best_model"
  
threshold <- 0.8

unit_tag <- "20km"
  
  
# load data ------------------------------------------------------------------- 


all_sqr_foi_orig <- readRDS(
  file.path(
    "output", 
    "predictions_world",
    model_tp,
    "FOI_best_all_squares.rds"))

age_struct <- read.csv(
  file.path("output", 
            "datasets",
            "country_age_structure.csv"), 
  header = TRUE) 

country_name <- read.csv(file.path("output", 
                                   "datasets",
                                   "adm0_country_names.csv"), 
                         header = TRUE)


# values for which there is age data available -------------------------------- 


all_sqr_foi <- inner_join(
  all_sqr_foi_orig,
  age_struct[, c("ID_0", "band_5_9")],
  by = c("ADM_0" = "ID_0"))


# calculate new variables -----------------------------------------------------


all_sqr_foi$p9 <- 1 - exp(-36 * all_sqr_foi$best)

all_sqr_foi$n9 <- all_sqr_foi$population * (all_sqr_foi$band_5_9 / 5)

# seropositives
all_sqr_foi$np9 <- all_sqr_foi$n9 * all_sqr_foi$p9    

# seronegatives
all_sqr_foi$nn9 <- all_sqr_foi$n9 - all_sqr_foi$np9 

all_sqr_foi$v9 <- ifelse(all_sqr_foi$p9 > threshold, all_sqr_foi$n9, 0)

all_sqr_foi$nv9 <- ifelse(all_sqr_foi$p9 > threshold, 1, 0)

all_sqr_foi_sum <- all_sqr_foi %>% 
  group_by(ADM_0) %>%
  summarise_at(c("population", "n9", "np9", "nn9", "v9", "nv9"), sum)

n_units <- count(all_sqr_foi, ADM_0)

all_sqr_foi_sum <- left_join(all_sqr_foi_sum, n_units)

final_table <- inner_join(country_name, all_sqr_foi_sum)

names(final_table)[names(final_table) == "NAME_ENGLI"] <- "country"
names(final_table)[names(final_table) == "n"] <- "units"

file_name <- sprintf("threshold_%s_%s%s", threshold * 100, unit_tag, ".csv")

write.csv(final_table, file_name, row.names = FALSE)
