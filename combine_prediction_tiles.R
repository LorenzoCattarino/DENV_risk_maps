options(didewin.cluster = "fi--didemrchnb")

CLUSTER <- FALSE

my_resources <- c(
  file.path("R", "utility_functions.R"))

my_pkgs <- "data.table"

context::context_log_start()
ctx <- context::context_save(packages = my_pkgs,
                             sources = my_resources,
                             root = "context")


# ---------------------------------------- define parameters 


in_pt <- file.path(
  "output", 
  "predictions",
  "best_model_20km_cw",
  "tile_sets_0_1667_deg")

out_pt <- file.path(
  "output", 
  "predictions",
  "best_model_20km_cw",
  "world_0_1667_deg")

out_fl_nm <- "pred_0_1667_deg_long.txt"


# ---------------------------------------- rebuild the queue


if (CLUSTER) {
  
  obj <- didewin::queue_didewin(ctx)
  
}else{
  
  context::context_load(ctx)
  
}


# ---------------------------------------- pre processing


fi <- list.files(in_pt, 
                 pattern = "^tile",
                 full.names = TRUE)


# ---------------------------------------- run


all_tiles <- lapply(fi, function(x) {
  fread(x,
        header = TRUE,
        sep = ",",
        na.strings = c("NA", "-1.#IND", "Peipsi", "Moskva", "IJsselmeer", "Zeeuwse meren"),
        fill = TRUE,
        data.table = FALSE)
  }
)

all_preds <- do.call("rbind", all_tiles)

sum(is.na(all_preds$mean_pred))

all_preds <- all_preds[!is.na(all_preds$mean_pred),]

dir.create(out_pt, FALSE, TRUE)

write.table(all_preds, 
            file.path(out_pt, out_fl_nm),
            row.names = FALSE,
            sep = ",")
