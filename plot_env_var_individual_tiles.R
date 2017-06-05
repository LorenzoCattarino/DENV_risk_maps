library(data.table)
library(fields)
library(RColorBrewer)

source(file.path("R", "prepare_datasets", "plot_data_one_tile.R"))
source(file.path("R", "prepare_datasets", "calculate_amplitude.R"))

# load data 
tiles <- read.csv(file.path("data", 
                            "env_variables", 
                            "plus60minus60_tiles.csv"), 
                  header = TRUE, 
                  sep = ",", 
                  stringsAsFactors = FALSE)

run_id <- 45

one_tile_ID <- tiles[run_id, "tile.id"]

one_tile <- fread(file.path("data",
                            "env_variables",
                            "all_sets", 
                            paste0("tile_", one_tile_ID, ".txt")), 
                  data.table = FALSE)

FTs_data <- c("DayTemp", "NightTemp", "EVI", "MIR", "RFE")

FT_term_to_plot <- c("const_term", "amplitude")

vars_to_plot <- apply(expand.grid(FT_term_to_plot, FTs_data), 1, function(x) paste(x[2],x[1], sep="_"))

vars_to_plot <- c(vars_to_plot, "population")

my_pal <- rev(colorRampPalette(brewer.pal(11, "RdBu"))(100))

sapply(vars_to_plot, 
       plot_tiles, 
       out_path = file.path("figures", 
                            "env_variables", 
                            "test_tiles"), 
       res = 1/120, 
       tile_set = one_tile, 
       tile_id = one_tile_ID,
       my_col = my_pal)
