
library(rgdal)
library(h2o)
library(ggplot2)

source(file.path("R", "prepare_datasets", "get_env_variables.R"))
source(file.path("R", "random_forest", "fit_h2o_RF_and_make_predictions.R"))
source(file.path("R", "utility_functions.R"))

my_h2o_ver <- "3.16.0.2"

if(packageVersion("h2o") != my_h2o_ver) install.packages(file.path("R_sources", "h2o_3.16.0.2.tar.gz"), repos = NULL, type = "source")
                 
                 
# define parameters -----------------------------------------------------------


parameters <- list(no_predictors = 9)

iso_code <- "BGD"

pop_var <- "pop"
alt_var <- "altitude"
FT_elem <- c("const_term",	"Re0",	"Im0",	"Re1",	"Im1")
FT_var <- c("RFE", "DayTemp", "NightTemp", "EVI", "MIR")
LC_var <- paste("lct1_2012001", c(seq(0, 16, 1), 254, 255), sep = "_")

dts_out_pt <- file.path("output", "seroprevalence") 
  
dts_out_nm <- "ProportionPositive_bangladesh_salje_env_var.csv"
  
  
# load data -------------------------------------------------------------------
  
  
salje_data <- read.csv(file.path("data",
                                       "seroprevalence",
                                       "ProportionPositive_bangladesh_salje.csv"))

shp <- readOGR(file.path("data", "shapefiles", "BGD_adm_shp"), paste0(iso_code, "_adm1"),
               stringsAsFactors = FALSE,
               integer64 = "allow.loss")

h2o.init()

RF_obj <- h2o.loadModel(file.path("output",
                                  "EM_algorithm",
                                  "best_fit_models",
                                  "FOI_best_model",
                                  "optimized_model_objects",
                                  "RF_obj.rds"))
  
predictor_rank <- read.csv(file.path("output", 
                                     "variable_selection", 
                                     "metropolis_hastings", 
                                     "exp_1", 
                                     "variable_rank_final_fits_exp_1.csv"),
                           stringsAsFactors = FALSE)

age_struct <- read.csv(file.path("output", 
                                 "datasets",
                                 "country_age_structure_mean.csv"), 
                       header = TRUE) 

# pre processing --------------------------------------------------------------


my_predictors <- predictor_rank$name[1:parameters$no_predictors]

salje_data$ISO <- iso_code
  
location_xy <- salje_data[, c("lon", "lat")]

xy_spdf <- SpatialPoints(location_xy, proj4string = shp@proj4string)


# plot ------------------------------------------------------------------------
   
   
png(file.path("figures", "data", "salje_bangl_points.png"),
    width = 10,
    height = 10,
    units = "cm",
    pointsize = 12,
    res = 300)

par(mar = c(0,0,0,0), oma = c(0,0,0,0))

plot(shp)

points(xy_spdf, pch = 21, cex = 0.6, bg = "red")

text(location_xy$lon, 
     location_xy$lat, 
     labels = salje_data$id_point, 
     pos = 3, 
     cex = 0.5)

dev.off()


# map overlay -----------------------------------------------------------------


overlay <- over(xy_spdf, shp)
country_numeric_code <- overlay$ID_0
adm_numeric_code <- overlay$ID_1
adm1_name <- overlay$NAME_1

salje_data$ID_0 <- country_numeric_code
salje_data$ID_1 <- adm_numeric_code
salje_data$adm1 <- adm1_name


# remove na adm name ----------------------------------------------------------


salje_data <- subset(salje_data, !is.na(ID_1))

salje_data$id_point <- seq_len(nrow(salje_data))


# extract env variables -------------------------------------------------------


salje_data_ls <- df_to_list(salje_data, use_names = TRUE)

number_of_variables <- length(c(alt_var, LC_var)) + (length(c(pop_var, FT_elem)) * length(FT_var))

extracted_var_values <- sapply(
  salje_data_ls, 
  get_env_variables, 
  no_vars = number_of_variables, 
  pop_vars = pop_var,
  alt_vars = alt_var, 
  FT_elements = FT_elem, 
  FT_data = FT_var, 
  LC_vars = LC_var, 
  admin_level = 1,
  my_path = file.path("output", "env_variables"))

salje_data_2 <- cbind(salje_data, t(extracted_var_values))

colnames(salje_data_2) <- c(names(salje_data), alt_var, 
                            apply(expand.grid(c(pop_var, FT_elem), FT_var), 
                                  1, 
                                  function(x) {paste(x[2],x[1], sep="_")}), LC_var)


# save dataset ----------------------------------------------------------------


write_out_csv(salje_data_2, dts_out_pt, dts_out_nm)


# make predictions ------------------------------------------------------------


salje_data$foi <- make_h2o_predictions(RF_obj, salje_data_2, my_predictors)


# seroprevalence --------------------------------------------------------------


age <- age_struct[age_struct$country == "Bangladesh", "mean"]
  
salje_data$o_j <- salje_data$nPos / salje_data$nAll

age_vec <- seq_len(100)

get_sero <- function(i, j){
  1 - (exp(-4 * i * j))
}         

get_sero(salje_data$foi[1], age_vec)
  
pred_serop <- t(vapply(salje_data$foi, get_sero, numeric(length(age_vec)), age_vec))

pred_serop <- rowMeans(pred_serop)

salje_data$p_j <- pred_serop 
  
cor.test(salje_data$o_j, salje_data$p_j)

ggplot(salje_data) +
  geom_point(aes(x = o_j, y = p_j))

write_out_csv(salje_data, dts_out_pt, "ProportionPositive_bangladesh_salje_pred.csv")
