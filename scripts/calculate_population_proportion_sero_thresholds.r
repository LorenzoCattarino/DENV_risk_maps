
library(dplyr)


# define parameters -----------------------------------------------------------


model_tp <- "FOI_best_model"
  
thresholds <- c(0.8, 0.85, 0.9)

unit_tags <- c("adm1", "adm2", "20km")

out_pth <- file.path("output", "predictions_world", model_tp)


# manual loop !!! -------------------------------------------------------------


threshold <- thresholds[3]

unit_tag <- unit_tags[3]


# load data ------------------------------------------------------------------- 


dts_name <- paste0("FOI_best_", unit_tag, ".rds")
  
all_sqr_foi_orig <- readRDS(
  file.path(
    "output", 
    "predictions_world",
    model_tp,
    dts_name))

age_struct <- read.csv(
  file.path("output", 
            "datasets",
            "country_age_structure.csv"), 
  header = TRUE) 

country_name <- read.csv(file.path("output", 
                                   "datasets",
                                   "adm0_country_names.csv"), 
                         header = TRUE)


# change column names ---------------------------------------------------------


names(all_sqr_foi_orig)[names(all_sqr_foi_orig) == "ADM_0"] <- "ID_0"

names(country_name)[names(country_name) == "ADM_0"] <- "ID_0"


# values for which there is age data available -------------------------------- 


all_sqr_foi <- inner_join(
  all_sqr_foi_orig,
  age_struct[, c("ID_0", "band_5_9")])


# start -----------------------------------------------------------------------


# seroprevalence
all_sqr_foi$p9 <- 1 - exp(-36 * all_sqr_foi$best)

# number of children
all_sqr_foi$n9 <- all_sqr_foi$population * (all_sqr_foi$band_5_9 / 5)

# number of seropositive children
all_sqr_foi$np9 <- all_sqr_foi$n9 * all_sqr_foi$p9    

# number of seronegative children
all_sqr_foi$nn9 <- all_sqr_foi$n9 - all_sqr_foi$np9 

# new variables
all_sqr_foi$v9 <- ifelse(all_sqr_foi$p9 > threshold, all_sqr_foi$n9, 0)
all_sqr_foi$nv9 <- ifelse(all_sqr_foi$p9 > threshold, 1, 0)

# sum by country
all_sqr_foi_sum <- all_sqr_foi %>% 
  group_by(ID_0) %>%
  summarise_at(c("population", "n9", "np9", "nn9", "v9", "nv9"), sum)

n_units <- count(all_sqr_foi, ID_0)

all_sqr_foi_sum <- left_join(all_sqr_foi_sum, n_units)


# calculate proportion of units and total population --------------------------


all_sqr_foi_sum$prop_units <- all_sqr_foi_sum$nv9 / all_sqr_foi_sum$n

all_sqr_foi_sum$prop_pop <- all_sqr_foi_sum$v9 / all_sqr_foi_sum$population


# join country names ----------------------------------------------------------


final_table <- inner_join(country_name, all_sqr_foi_sum)


# save ------------------------------------------------------------------------


names(final_table)[names(final_table) == "NAME_ENGLI"] <- "country"
names(final_table)[names(final_table) == "n"] <- "units"

file_name <- sprintf("threshold_%s_%s%s", threshold * 100, unit_tag, ".csv")

write.csv(final_table, 
          file.path(out_pth, file_name), 
          row.names = FALSE)
