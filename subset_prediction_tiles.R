options(didewin.cluster = "fi--didemrchnb")

CLUSTER <- TRUE

my_resources <- c(
  file.path("R", "utility_functions.R"),
  file.path("R", "prepare_datasets", "wrapper_to_subset_tile_preds.R"))

my_pkgs <- c("data.table", "dplyr")

context::context_log_start()
ctx <- context::context_save(packages = my_pkgs,
                             sources = my_resources,
                             root = "context")


# ---------------------------------------- are you using the cluster?


if (CLUSTER) {
  
  #config <- didewin::didewin_config(template = "24Core")
  obj <- didewin::queue_didewin(ctx)
  
} else {
  
  context::context_load(ctx)
  #context::start_parallel_cluster(8, ctx)
  
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
  file.path("output", "All_FOI_estimates_linear_env_var.csv"),
  header = TRUE)


# ---------------------------------------- create objects needed for run 


fi <- list.files(in_pred_path, 
                 pattern = "^tile",
                 full.names = TRUE)


# ---------------------------------------- pre process the original foi dataset


foi_data <- subset(foi_data, ISO != "PYF" & ISO != "HTI")

foi_data <- subset(foi_data, type != "pseudoAbsence")
  
ad_adm <- foi_data %>% group_by_(.dots = c("ID_0", "ID_1"))

o_j <- ad_adm %>% summarise(o_j = mean(FOI))

names(o_j)[names(o_j) == "ID_0"] <- group_fields[1]
names(o_j)[names(o_j) == "ID_1"] <- group_fields[2]


# ---------------------------------------- submit jobs


if (CLUSTER) {
  
  sub_tile <- queuer::qlapply(
    fi,
    wrapper_to_subset_prediction_tiles,
    obj,
    foi_dts = o_j, 
    grp_flds = group_fields,
    out_path = sub_tls_path)
  
}else{
  
  sub_tile <- lapply(
    fi[1],
    wrapper_to_subset_prediction_tiles,
    foi_dts = o_j, 
    grp_flds = group_fields,
    out_path = sub_tls_path)
  
}

if (!CLUSTER) {
  context:::stop_parallel_cluster()
}
