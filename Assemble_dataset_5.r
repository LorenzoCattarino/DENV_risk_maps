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
