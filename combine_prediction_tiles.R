options(didehpc.cluster = "fi--didemrchnb")

CLUSTER <- FALSE

my_resources <- c(
  file.path("R", "utility_functions.R"))

my_pkgs <- "data.table"

context::context_log_start()
ctx <- context::context_save(path = "context",
                             sources = my_resources,
                             packages = my_pkgs)


# ---------------------------------------- define parameters 


in_pt <- file.path(
  "output", 
  "predictions",
  "boot_model_20km_cw",
  "tile_sets_0_0083_deg")

out_pt <- file.path(
  "output", 
  "predictions",
  "boot_model_20km_cw")

out_fl_nm <- "pred_0_0083_deg_long.txt"


# ---------------------------------------- rebuild the queue


if (CLUSTER) {
  
  config <- didehpc::didehpc_config(template = "24Core")
  obj <- didehpc::queue_didehpc(ctx, config = config)
  
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
