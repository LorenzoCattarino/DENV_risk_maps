options(didewin.cluster = "fi--didemrchnb")

CLUSTER <- TRUE

my_resources <- c(
  file.path("R", "prepare_datasets", "filter_and_resample.R"),
  file.path("R", "prepare_datasets", "grid_up_foi_dataset.R"),
  file.path("R", "prepare_datasets", "average_up.R"))

my_pkgs <- c("data.table", "dplyr")

context::context_log_start()
ctx <- context::context_save(packages = my_pkgs,
                             sources = my_resources,
                             root = "context")


# ---------------------------------------- define parameters


in_pt <- file.path("data", "gadm_codes")

group_fields <- c("ADM_0", "ADM_1", "cell", "lat.grid", "long.grid")

gr_size <- 20
  
new_res <- (1 / 120) * gr_size


# ---------------------------------------- are you using the cluster? 


if (CLUSTER) {
  
  obj <- didewin::queue_didewin(ctx)

} else {
  
  context::context_load(ctx)

}


# ---------------------------------------- load data


all_predictors <- read.table(
  file.path("output", 
            "datasets", 
            "all_predictors.txt"), 
  header = TRUE, 
  stringsAsFactors = FALSE)

foi_data <- read.csv(
  file.path("output", 
            "dengue_dataset", 
            "All_FOI_estimates_linear_env_var.csv"),
  stringsAsFactors = FALSE)


# ---------------------------------------- pre processing


# remove outliers 
foi_data <- subset(foi_data, ISO != "PYF" & ISO != "HTI")

names(foi_data)[names(foi_data) == "ID_0"] <- "ADM_0"
names(foi_data)[names(foi_data) == "ID_1"] <- "ADM_1"

foi_data[foi_data$type!= "pseudoAbsence", "type"] <- "data"


# ----------------------------------------

# get unique combinations of adm0, adm1 and (data) type

#  NOTE: a particular adm0-adm1 combination can be either data or pseudoAbsence type
#  because creation of pseudoAbsences has been done following adm1-based classification of dengue occurrence
#  so no duplicate adm0-adm1 combinations exist with regard to type 

# ----------------------------------------

foi_data <- foi_data[!duplicated(foi_data[, c("ADM_0", "ADM_1")]), c("ADM_0", "ADM_1", "type")]

var_names <- all_predictors$variable

fi <- list.files(in_pt, 
                 pattern = "^tile",
                 full.names = TRUE)


# ---------------------------------------- submit jobs


if (CLUSTER) {
  
  pxl_job <- queuer::qlapply(
    fi,
    filter_and_resample,
    obj,
    foi_dts = foi_data, 
    env_var_names = var_names, 
    grp_flds = group_fields, 
    grid_size = new_res)

}else{
  
  pxl_job <- lapply(
    fi[1],
    filter_and_resample,
    foi_dts = foi_data, 
    env_var_names = var_names, 
    grp_flds = group_fields, 
    grid_size = new_res)
  
}

# t <- obj$enqueue(
#   wrapper_to_tiles_to_dts(
#     fi[1],
#     foi_dts = foi_data,
#     env_var_names = var_names,
#     grp_flds = group_fields,
#     grid_size = new_res))
