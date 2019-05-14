library(dplyr)
source(file.path("R", "prepare_datasets", "average_up.R"))

set.seed(45)

example_df <- data.frame(ID_0 = 1, ID_1 = 1, tile = c(1, 2), value = runif(10), population = sample(1:100, 10))

example_spl <- split(example_df, example_df$tile)

wm_splt <- lapply(example_spl, average_up, c("ID_0", "ID_1"), "value")

wm_out <- do.call("rbind", wm_splt)

average_up(wm_out, c("ID_0", "ID_1"), "value")

average_up(example_df, c("ID_0", "ID_1"), "value")

