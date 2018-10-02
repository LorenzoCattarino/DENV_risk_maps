
# plot proportional reduction in infections, cases and hospitalized cases 
# from wolbachia and vaccination 


library(dplyr)
library(ggplot2)

out_fig_path <- file.path("figures", 
                          "predictions_world", 
                          "bootstrap_models")

my_var_name <- "infections"

intervention_name <- "wolbachia"

summary_table_orig <- read.csv(file.path("output", 
                                    "predictions_world", 
                                    "bootstrap_models",
                          paste0("prop_change_",my_var_name,"_wolbachia.csv")),
                          header = TRUE)

summary_table <- subset(summary_table_orig, treatment != 1)


# plotting ------------------------------------------------------------------


y_values <- seq(0, 1, by = 0.2)

summary_table$treatment <- as.factor(summary_table$treatment)

p <- ggplot(summary_table, aes(x = treatment, y = mean, fill = treatment, ymin = lCI, ymax = uCI)) +
  geom_bar(stat = "identity", position = "dodge", width = 1) +
  geom_errorbar(width = .25, position = position_dodge(.9)) +
  facet_grid(. ~ phi_set_id) +
  scale_fill_manual(values = c("lightskyblue1", "lightskyblue4"),
                    labels = c("50%", "70%"),
                    guide = guide_legend(title = expression('R'['0']*' reduction'),
                                         keywidth = 2,
                                         keyheight = 2)) +
  xlab(NULL) +
  scale_y_continuous("Reduction in infections",
                     breaks = y_values,
                     labels = paste0(y_values * 100, "%"),
                     limits = c(min(y_values), max(y_values))) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(size = 12),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
        strip.text.x = element_text(size = 8))

dir.create(out_fig_path, FALSE, TRUE)

barplot_fl_nm <- paste0("proportional_reduction_in_", my_var_name, "_", intervention_name, ".png")

png(file.path(out_fig_path, barplot_fl_nm),
    width = 17,
    height = 9,
    units = "cm",
    pointsize = 12,
    res = 300)

print(p)

dev.off()
