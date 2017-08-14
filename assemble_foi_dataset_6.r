# Removes duplicate pseudo absence points in each adm0-adm1 combination
# It helps containing the number of pseudo absences when disaggregating the dataset

# load packages
library(maptools)

pap <- read.csv(
  file.path("output", 
            "datasets", 
            "pseudo_absence_points_NUM_CODES.csv"), 
  header = TRUE, 
  sep = ",", 
  stringsAsFactors = FALSE)

dup <- duplicated(pap[, c("ID_0", "ID_1")])

pap_2 <- pap[!dup, ]

write.csv(pap_2,
          file.path("output", 
                    "datasets", 
                    "pseudo_absence_points_NUM_CODES_sub.csv"), 
          row.names = FALSE)
