library(data.table)
library(fields)
library(RColorBrewer)

source(file.path("R", "plotting", "plot_data_one_tile.R"))
source(file.path("R", "prepare_datasets", "calculate_amplitude.R"))

# load data 
tiles <- read.csv(file.path("data", 
                            "env_variables", 
                            "plus60minus60_tiles.csv"), 
                  header = TRUE, 
                  sep = ",", 
                  stringsAsFactors = FALSE)

run_id <- 72

out_pt <- file.path("figures", 
                    "env_variables", 
                    "test_tiles")

one_tile_ID <- tiles[run_id, "tile.id"]

one_tile <- fread(file.path("output",
                            "env_variables",
                            "tile_set_2", 
                            paste0("tile_", one_tile_ID, ".txt")), 
                  header = TRUE,
                  sep = ",",
                  na.strings = c("NA", "-1.#IND", "Peipsi", "Moskva", "IJsselmeer", "Zeeuwse meren"),
                  fill = TRUE,
                  data.table = FALSE)

FTs_data <- c("DayTemp", "NightTemp", "EVI", "MIR", "RFE")

FT_term_to_plot <- c("const_term", "amplitude", "Re0", "Im0", "Re1", "Im1")

all_combs <- expand.grid(FTs_data, FT_term_to_plot)

vars_to_plot <- apply(all_combs, 1, function(x) paste(x[1], x[2], sep = "_"))

vars_to_plot <- c(vars_to_plot, "population", "travel_time", "TSI", "aedes_gen")

my_pal <- rev(colorRampPalette(brewer.pal(11, "RdBu"))(100))

sapply(vars_to_plot, 
       plot_tiles, 
       out_path = out_pt, 
       res = 1/120, 
       tile_set = one_tile, 
       tile_id = one_tile_ID,
       my_col = my_pal)
