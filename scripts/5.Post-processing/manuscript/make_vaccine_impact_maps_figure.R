# Maps of vaccine impact

# 1) proportional reduction in cases  
# 2) total numbers of cases, and 
# 3) number of dengue free countries 

# for an intervention with varying blocking effect on R0, AND 

# 4) map of proportional reduction in cases for 70% reduction effect (R0 scenario 1)
# 5) map of proportional reduction in cases for 70% reduction effect (R0 scenario 2)


library(sf)
library(tidyverse)

source(file.path("R", "create_parameter_list.R"))


# define parameters ----------------------------------------------------------- 


extra_prms <- list(id = 2,
                   coord_limits = c(-130, 180, -60, 38))


# define variables ------------------------------------------------------------


parameters <- create_parameter_list(extra_params = extra_prms)

model_type <- paste0("model_", parameters$id)

out_pt <- file.path("figures", "predictions_world")

out_name <- "maps_vaccine_impact.png"

x1 <- parameters$coord_limits[1]
x2 <- parameters$coord_limits[2]
y1 <- parameters$coord_limits[3]
y2 <- parameters$coord_limits[4]

my_col <- rev(colorRamps::matlab.like(100))
# my_col <- viridis::plasma(100)
# my_col <- viridis::inferno(100)
# my_col <- viridis::viridis(100)
# my_col <- colorspace::diverge_hcl(11,
#                                   h = c(250, 10), 
#                                   c = 100, 
#                                   l = c(20, 95), 
#                                   power = c(0.7, 1.7))

in_path <- file.path("output",
                     "predictions_world",
                     "bootstrap_models",
                     model_type)

in_path_2 <- file.path("output",
                       "predictions_world",
                       "bootstrap_models",
                       model_type,
                       "adm_1")


# load data -------------------------------------------------------------------


pr_cases_R01_a <- readRDS(file.path(in_path, "C_pr_1_vaccine_8_adm_mean.rds"))

pr_cases_R01_b <- readRDS(file.path(in_path, "C_pr_1_vaccine_20_adm_mean.rds"))

pr_cases_R01_c <- readRDS(file.path(in_path_2, "C_pr_1_vaccine_8_adm_mean.rds"))

pr_cases_R02_a <- readRDS(file.path(in_path, "C_pr_2_vaccine_8_adm_mean.rds"))

pr_cases_R02_b <- readRDS(file.path(in_path, "C_pr_2_vaccine_20_adm_mean.rds"))

pr_cases_R02_c <- readRDS(file.path(in_path_2, "C_pr_2_vaccine_8_adm_mean.rds"))

countries <- st_read(dsn = file.path("output", "shapefiles"), 
                     layer = "gadm28_adm0_eras")

adm_shp <- st_read(dsn = file.path("output", "shapefiles"), 
                   layer = "gadm28_adm1_eras",
                   stringsAsFactors = FALSE)


# pre processing --------------------------------------------------------------


countries <- countries[!countries$NAME_ENGLI == "Caspian Sea", ]

adm_shp_2 <- adm_shp %>% 
  
  left_join(pr_cases_R01_a[, c("ID_0", "ID_1", "mean")], by = c("ID_0", "ID_1")) %>%
  
  rename(pr_R0_1_a = mean) %>%
  
  left_join(pr_cases_R01_b[, c("ID_0", "ID_1", "mean")], by = c("ID_0", "ID_1")) %>%
  
  rename(pr_R0_1_b = mean) %>%
  
  left_join(pr_cases_R01_c[, c("ID_0", "ID_1", "mean")], by = c("ID_0", "ID_1")) %>%
  
  rename(pr_R0_1_c = mean) %>%
  
  left_join(pr_cases_R02_a[, c("ID_0", "ID_1", "mean")], by = c("ID_0", "ID_1")) %>%
  
  rename(pr_R0_2_a = mean) %>%
  
  left_join(pr_cases_R02_b[, c("ID_0", "ID_1", "mean")], by = c("ID_0", "ID_1")) %>%
  
  rename(pr_R0_2_b = mean) %>%
  
  left_join(pr_cases_R02_c[, c("ID_0", "ID_1", "mean")], by = c("ID_0", "ID_1")) %>%
  
  rename(pr_R0_2_c = mean)


adm_shp_2_long <- adm_shp_2 %>%
  
  gather("scenario", "pr", pr_R0_1_a:pr_R0_2_c)


# -----------------------------------------------------------------------------


adm_shp_2_long$scenario <- factor(adm_shp_2_long$scenario, 
                                  levels = c("pr_R0_1_a",
                                             "pr_R0_1_b",
                                             "pr_R0_1_c",
                                             "pr_R0_2_a",
                                             "pr_R0_2_b",
                                             "pr_R0_2_c"))

data_text <- data.frame(scenario = c("pr_R0_1_a",
                                     "pr_R0_1_b",
                                     "pr_R0_1_c",
                                     "pr_R0_2_a",
                                     "pr_R0_2_b",
                                     "pr_R0_2_c"),
                        label = c("A", "B", "C", "D", "E", "F"))

z_vals <- seq(0, 0.4, 0.2)

p <- ggplot(adm_shp_2_long) +
  geom_sf(mapping = aes(fill = pr), color = NA) +
  facet_wrap(~ scenario, ncol = 2, dir = "v") +
  geom_text(data = data_text, aes(x = -125, y = 31, label = label), size = 5) +
  coord_sf(datum = NA, xlim = c(x1, x2), ylim = c(y1, y2), expand = FALSE) +
  scale_fill_gradientn(breaks = z_vals,
                       labels = paste0(z_vals * 100, "%"),
                       limits = c(min(z_vals), max(z_vals)),
                       colours = my_col, 
                       na.value = "grey80",
                       guide = guide_colourbar(title = "Cases reduction",
                                               title.position = "left",
                                               title.theme = element_text(size = 8, angle = 90),
                                               barwidth = 0.5, 
                                               barheight = 3.5)) +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        strip.text.x = element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "cm"),
        legend.justification = c(0, 0), 
        legend.position = c(0, 0),
        legend.text = element_text(size = 7, margin = margin(l = -0.1, unit = "cm")),
        legend.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "cm"))

dir.create(out_pt, FALSE, TRUE)

png(file.path(out_pt, out_name),
    width = 18.4,
    height = 9,
    units = "cm",
    pointsize = 12,
    res = 300)

print(p)

dev.off()
