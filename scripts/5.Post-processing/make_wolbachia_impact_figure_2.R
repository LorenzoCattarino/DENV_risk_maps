# Alternative plot 

# 1) proportional reduction in cases  
# 2) total numbers of cases, and 
# 3) number of dengue free countries 

# for an intervention with varying blocking effect on R0, AND 

# 4) map of proportional reduction in cases for 70% reduction effect (R0 scenario 1)
# 5) map of proportional reduction in cases for 70% reduction effect (R0 scenario 2)


library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(grid)
library(gridExtra)
library(gtable)

source(file.path("R", "plotting", "extract_gplot_legend.R"))


# define parameters -----------------------------------------------------------


sf_vals <- seq(0.9, 0.1, -0.1)

coord_limits = c(-130, 180, -60, 38)

leg_titles <- c(expression('R'['0']*' reduction'), "Screening age")

burden_measures <- "cases"

out_fig_path <- file.path("figures", 
                          "predictions_world", 
                          "bootstrap_models",
                          "general_intervention")

interventions <- "wolbachia"


# define variables ------------------------------------------------------------


sf_vals_perc <- (1 - sf_vals) * 100

leg_labels <- list(paste0(sf_vals_perc, "%"), c("9", "16"))

my_col <- brewer.pal(9, "YlGnBu")


# plotting --------------------------------------------------------------------


for (j in seq_along(burden_measures)) {
  
  my_var_name <- burden_measures[j]
  
  intervention_name <- interventions
  
  summary_table_orig <- read.csv(file.path("output", 
                                           "predictions_world", 
                                           "bootstrap_models",
                                           paste0("prop_change_", my_var_name, "_", intervention_name, ".csv")),
                                 header = TRUE)
  
  summary_table <- subset(summary_table_orig, treatment != 1 & phi_set_id != "FOI")
  summary_table$treatment <- 1 - summary_table$treatment
  
  y_values <- seq(0, 1, 0.2)
  
  p1 <- ggplot(summary_table) +
    geom_ribbon(aes(x = treatment, 
                    ymin = lCI, 
                    ymax = uCI, 
                    fill = phi_set_id), 
                alpha = 0.3) +
    geom_line(aes(x = treatment, y = mean, colour = phi_set_id)) +
    geom_vline(xintercept = c(0.3, 0.7), linetype = c("dotted", "dashed"), size = 1) +
    scale_fill_manual(values = c("red", "blue", "green"),
                      aesthetics = c("fill", "colour"),
                      guide = guide_legend(title = "Infectiousness",
                                           keywidth = 1,
                                           keyheight = 1)) +
    scale_x_continuous(NULL,
                       breaks = 1 - sf_vals,
                       labels = paste0(sf_vals_perc, "%")) +
    scale_y_continuous("Reduction in cases",
                       breaks = y_values,
                       labels = paste0(y_values * 100, "%"),
                       limits = c(min(y_values), max(y_values)),
                       expand = expand_scale(mult = c(0, .05))) +
    theme_bw() +
    theme(axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          #plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
          strip.text.x = element_text(size = 8),
          axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0))) +
    labs(tag = LETTERS[1])
  
  tot_num_table_orig <- read.csv(file.path("output", 
                                           "predictions_world", 
                                           "bootstrap_models",
                                           paste0("total_", my_var_name, "_", intervention_name, ".csv")),
                                 header = TRUE)
  
  tot_num_table <- subset(tot_num_table_orig, treatment != 1 & phi_set_id != "FOI")
  tot_num_table$treatment <- 1 - tot_num_table$treatment
  
  y_values <- pretty(tot_num_table$uCI)
  
  p2 <- ggplot(tot_num_table) +
    geom_ribbon(aes(x = treatment, 
                    ymin = lCI, 
                    ymax = uCI, 
                    fill = phi_set_id), 
                alpha = 0.3) +
    geom_line(aes(x = treatment, y = mean, colour = phi_set_id)) +
    geom_vline(xintercept = c(0.3, 0.7), linetype = c("dotted", "dashed"), size = 1) +
    scale_fill_manual(values = c("red", "blue", "green"),
                      aesthetics = c("fill", "colour"),
                      guide = guide_legend(title = "Infectiousness assumption",
                                           keywidth = 1,
                                           keyheight = 1)) +
    scale_x_continuous(NULL,
                       breaks = 1 - sf_vals,
                       labels = paste0(sf_vals_perc, "%")) +
    scale_y_continuous(paste0("Number of ", my_var_name, " (Million)"),
                       breaks = y_values,
                       labels = format(y_values / 1000000, scientific = F),
                       limits = c(min(y_values), max(y_values)),
                       expand = expand_scale(mult = c(0, .05))) +
    theme_bw() +
    theme(axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          #plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
          strip.text.x = element_text(size = 8),
          axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0))) +
    labs(tag = LETTERS[2])
  
}

dengue_free_table_orig <- read.csv(file.path("output", 
                                             "predictions_world", 
                                             "bootstrap_models",
                                             "dengue_free_countries_wolbachia.csv"),
                                   header = TRUE)

dengue_free_table <- subset(dengue_free_table_orig, treatment != 1 & phi_set_id != "FOI")
dengue_free_table$treatment <- 1 - dengue_free_table$treatment

y_values <- pretty(dengue_free_table$mean)

p3 <- ggplot(dengue_free_table) +
  geom_ribbon(aes(x = treatment, 
                  ymin = lCI, 
                  ymax = uCI, 
                  fill = phi_set_id), 
              alpha = 0.3) +
  geom_line(aes(x = treatment, y = mean, colour = phi_set_id)) +
  geom_vline(xintercept = c(0.3, 0.7), linetype = c("dotted", "dashed"), size = 1) +
  scale_fill_manual(values = c("red", "blue", "green"),
                    aesthetics = c("fill", "colour"),
                    guide = guide_legend(title = "Infectiousness assumption",
                                         keywidth = 1,
                                         keyheight = 1)) +
  scale_x_continuous(leg_titles[[1]],
                     breaks = 1 - sf_vals,
                     labels = paste0(sf_vals_perc, "%")) +
  scale_y_continuous(paste0("Dengue-free countries"),
                     breaks = y_values,
                     labels = y_values,
                     limits = c(min(dengue_free_table$lCI), max(y_values)),
                     expand = expand_scale(mult = c(0, .05))) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        #plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
        strip.text.x = element_text(size = 8),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0))) +
  labs(tag = LETTERS[3])

mylegend <- g_legend(p1)

dir.create(out_fig_path, FALSE, TRUE)

barplot_fl_nm <- paste0("multi_output_", my_var_name, "_", intervention_name, ".png")

png(file.path(out_fig_path, barplot_fl_nm),
    width = 17,
    height = 16,
    units = "cm",
    pointsize = 12,
    res = 300)


# -----------------------------------------------------------------------------


countries <- readOGR(dsn = file.path("output", "shapefiles"), 
                     layer = "gadm28_adm0_eras")

bbox <- readOGR(dsn = file.path("data", "shapefiles", "ne_50m_graticules_all"), 
                layer = "ne_50m_wgs84_bounding_box") 

my_ext <- extent(coord_limits)

gr_size <- parameters$resample_grid_size

res <- (1 / 120) * gr_size

lats <- seq(-90, 90, by = res)
lons <- seq(-180, 180, by = res)

countries <- countries[!countries@data$NAME_ENGLI == "Caspian Sea", ]

fl_ex <- file.exists(file.path("output", "shapefiles", "gadm28_adm0_eras_cropped.shp"))

if(fl_ex){
  
  countries_cropped <- readOGR(dsn = file.path("output", "shapefiles"),
                               layer = "gadm28_adm0_eras_cropped",
                               stringsAsFactors = FALSE,
                               integer64 = "allow.loss")
  
} else {
  
  countries_cropped <- crop(countries, my_ext)
  
  writeOGR(countries_cropped, 
           dsn = file.path("output", "shapefiles"), 
           layer = "gadm28_adm0_eras_cropped", 
           driver = "ESRI Shapefile")
  
}


countries_df <- fortify(countries_cropped)

bbox <- crop(bbox, my_ext)

bbox_df <- fortify(bbox)

pred <- readRDS(file.path("output",
                          "predictions_world",
                          "bootstrap_models",
                          "model_4",
                          "C_pr_1_wolbachia_32_mean.rds"))

pred_mat <- prediction_df_to_matrix(lats, lons, pred, "mean")

pred_mat_ls <- list(x = lons,
                    y = lats,
                    z = pred_mat)

pred_r_mat <- raster(pred_mat_ls)

pred_r_mat <- crop(pred_r_mat, my_ext)

pred_r_spdf <- as(pred_r_mat, "SpatialPixelsDataFrame")

pred_r_df <- as.data.frame(pred_r_spdf)

pred_map <- make_nice_map(bbox_df, countries_df, pred_r_df, pred_leg_val, parms, my_col, "grey70", "FOI")


# -----------------------------------------------------------------------------


pred2 <- readRDS(file.path("output",
                          "predictions_world",
                          "bootstrap_models",
                          "model_4",
                          "C_pr_2_wolbachia_32_mean.rds"))

pred_mat <- prediction_df_to_matrix(lats, lons, pred2, "mean")

pred2_mat_ls <- list(x = lons,
                    y = lats,
                    z = pred_mat)

pred2_r_mat <- raster(pred2_mat_ls)

pred2_r_mat <- crop(pred2_r_mat, my_ext)

pred2_r_spdf <- as(pred2_r_mat, "SpatialPixelsDataFrame")

pred2_r_df <- as.data.frame(pred2_r_spdf)


# -----------------------------------------------------------------------------


g1 <- ggplotGrob(p1 + theme(legend.position = "none"))
g2 <- ggplotGrob(p2 + theme(legend.position = "none"))
g3 <- ggplotGrob(p3 + theme(legend.position = "none"))
g <- rbind(g1, g2, g3, size = "first")
g$widths <- unit.pmax(g1$widths, g2$widths, g3$widths)

grid.arrange(g, mylegend, ncol = 2, widths = c(8, 3))

dev.off()
