options(didehpc.cluster = "fi--didemrchnb")

CLUSTER <- TRUE

my_resources <- c(
  file.path("R", "utility_functions.r"),
  file.path("R", "prepare_datasets", "wrapper_to_subset_tile_preds.R"))

my_pkgs <- c("data.table", "dplyr")

context::context_log_start()
ctx <- context::context_save(path = "context",
                             sources = my_resources,
                             packages = my_pkgs)


# ---------------------------------------- are you using the cluster?


if (CLUSTER) {
  
  obj <- didehpc::queue_didehpc(ctx)
  
} else {
  
  context::context_load(ctx)
  
}


# ---------------------------------------- define parameters


group_fields <- c("ADM_0", "ADM_1")

in_pred_path <- file.path(
  "output", 
  "predictions", 
  "best_model_20km_cw",
  "tile_sets_0_0083_deg")

sub_tls_path <- file.path(
  "output",
  "predictions",
  "best_model_20km_cw",
  "tile_sets_0_0083_deg_sub")


# ---------------------------------------- load data 


# original dataset
foi_data <- read.csv(
  file.path("output", "foi", "All_FOI_estimates_linear_env_var.csv"),
  header = TRUE)


# ---------------------------------------- create objects needed for run 


fi <- list.files(in_pred_path, 
                 pattern = "^tile",
                 full.names = TRUE)


# ---------------------------------------- pre process the original foi dataset


foi_data <- subset(foi_data, type != "pseudoAbsence")
  
names(foi_data)[names(foi_data) == "FOI"] <- "o_j"
names(foi_data)[names(foi_data) == "ID_0"] <- group_fields[1]
names(foi_data)[names(foi_data) == "ID_1"] <- group_fields[2]


# ---------------------------------------- submit one job 


# t <- obj$enqueue(
#   wrapper_to_subset_tile_predictions(
#     fi[1],
#     foi_dts = foi_data,
#     grp_flds = group_fields,
#     out_path = sub_tls_path))


# ---------------------------------------- submit all jobs


if (CLUSTER) {
  
  sub_tile <- queuer::qlapply(
    fi,
    wrapper_to_subset_tile_predictions,
    obj,
    foi_dts = foi_data, 
    grp_flds = group_fields,
    out_path = sub_tls_path)
  
}else{
  
  sub_tile <- lapply(
    fi[1],
    wrapper_to_subset_tile_predictions,
    foi_dts = foi_data, 
    grp_flds = group_fields,
    out_path = sub_tls_path)
  
}

if (!CLUSTER) {
  context:::stop_parallel_cluster()
}
