# Plot 

# 1) proportional reduction in cases  
# 2) total numbers of cases, and 
# 3) number of dengue free countries 

# for an intervention with varying blocking effect on R0 


library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(grid)
library(gridExtra)
library(gtable)

source(file.path("R", "plotting", "extract_gplot_legend.R"))


# define parameters -----------------------------------------------------------


sf_vals <- seq(0.9, 0.1, -0.1)

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
    geom_vline(xintercept = c(0.3, 0.7), linetype = c("dashed", "dashed"), size = 0.5) +
    scale_fill_manual(values = c("red", "blue", "green"),
                      aesthetics = c("fill", "colour"),
                      guide = guide_legend(title = "Infectiousness",
                                           keywidth = 1,
                                           keyheight = 1,
                                           label.theme = element_text(size = 10))) +
    scale_x_continuous(breaks = 1 - sf_vals) +
    scale_y_continuous("Reduction in \nnumber of cases",
                       breaks = y_values,
                       labels = paste0(y_values * 100, "%"),
                       limits = c(min(y_values), max(y_values)),
                       expand = expand_scale(mult = c(0, .05))) +
    theme_bw() +
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.y = element_text(size = 10),
          axis.title.y = element_text(margin = unit(c(0, 0, 0, 0), "cm")),
          plot.margin = unit(c(0, 0.1, 0.02, 0.1), "cm"),
          strip.text.x = element_text(size = 8),
          legend.position = "bottom") +
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
    geom_vline(xintercept = c(0.3, 0.7), linetype = c("dashed", "dashed"), size = 0.5) +
    scale_fill_manual(values = c("red", "blue", "green"),
                      aesthetics = c("fill", "colour"),
                      guide = guide_legend(title = "Infectiousness assumption",
                                           keywidth = 1,
                                           keyheight = 1)) +
    scale_x_continuous(breaks = 1 - sf_vals) +
    scale_y_continuous(paste0("Number of ", my_var_name, "\n(millions)"),
                       breaks = y_values,
                       labels = format(y_values / 1000000, scientific = F),
                       expand = expand_scale(mult = c(0, .05))) +
    coord_cartesian(ylim = c(min(y_values), 65000000)) +
    theme_bw() +
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.y = element_text(size = 10),
          axis.title.y = element_text(margin = unit(c(0, 0, 0, 0), "cm")),
          plot.margin = unit(c(0, 0.1, 0.02, 0.1), "cm"),
          strip.text.x = element_text(size = 8)) +
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
  geom_vline(xintercept = c(0.3, 0.7), linetype = c("dashed", "dashed"), size = 0.5) +
  scale_fill_manual(values = c("red", "blue", "green"),
                    aesthetics = c("fill", "colour"),
                    guide = guide_legend(title = "Infectiousness assumption",
                                         keywidth = 1,
                                         keyheight = 1)) +
  scale_x_continuous(leg_titles[[1]],
                     breaks = 1 - sf_vals,
                     labels = paste0(sf_vals_perc, "%")) +
  scale_y_continuous(paste0("Dengue-free\ncountries"),
                     breaks = y_values,
                     labels = y_values,
                     expand = expand_scale(mult = c(0, .05))) +
  coord_cartesian(ylim = c(min(y_values), 130)) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        plot.margin = unit(c(0, 0.1, 0.02, 0.1), "cm"),
        axis.title.y = element_text(margin = unit(c(0, 0, 0, 0), "cm")),
        strip.text.x = element_text(size = 8),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0))) +
  labs(tag = LETTERS[3])

mylegend <- g_legend(p1)

dir.create(out_fig_path, FALSE, TRUE)

barplot_fl_nm <- paste0("multi_output_", my_var_name, "_", intervention_name, ".png")

png(file.path(out_fig_path, barplot_fl_nm),
    width = 10,
    height = 14,
    units = "cm",
    pointsize = 12,
    res = 300)

g1 <- ggplotGrob(p1 + theme(legend.position = "none"))
g2 <- ggplotGrob(p2 + theme(legend.position = "none"))
g3 <- ggplotGrob(p3 + theme(legend.position = "none"))
g <- rbind(g1, g2, g3, size = "first")
g$widths <- unit.pmax(g1$widths, g2$widths, g3$widths)

grid.arrange(g, mylegend, nrow = 2, widths = 6, heights = c(15, 1))

dev.off()
