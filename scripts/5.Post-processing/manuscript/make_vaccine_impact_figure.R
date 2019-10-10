
library(ggplot2)

intervention_name <- "vaccine"
my_var_name <- "cases"
out_fig_path <- file.path("figures", 
                          "predictions_world", 
                          "bootstrap_models")
scale_fill_vals <- c("lightskyblue1", "lightskyblue4", "deepskyblue")
leg_title <- "Screening age"
leg_labels <- c("9", "16", "Optimal")
y_axis_title <- "Cases reduction"
screening_ages <- c(9, 16, 0)
y_values <- seq(0, 0.45, 0.1)


# load data -------------------------------------------------------------------


summary_table_all <- read.csv(file.path("output", 
                                        "predictions_world", 
                                        "bootstrap_models", 
                                        "prop_change_cases_vaccine.csv"))


# -----------------------------------------------------------------------------


summary_table_all$treatment <- factor(summary_table_all$treatment, 
                                      levels = screening_ages)


p <- ggplot(summary_table_all, aes(x = treatment, y = mean, fill = treatment, ymin = lCI, ymax = uCI)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.5), width = 0.9) +
  geom_errorbar(width = .15, position = position_dodge(.9)) +
  facet_grid(. ~ phi_set_id) +
  scale_fill_manual(values = scale_fill_vals,
                    labels = leg_labels,
                    guide = guide_legend(title = leg_title,
                                         keywidth = 1.3,
                                         keyheight = 1.3,
                                         label.theme = element_text(size = 10))) +
  xlab(NULL) +
  scale_y_continuous(y_axis_title,
                     breaks = y_values,
                     labels = paste0(y_values * 100, "%"),
                     limits = c(min(y_values), max(y_values)),
                     expand = expand_scale(mult = c(0, .05))) +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(size = 12),
        plot.margin = unit(c(0, 0, 0.05, 0), "cm"),
        strip.text.x = element_text(size = 10),
        legend.position = "bottom")

dir.create(out_fig_path, FALSE, TRUE)

barplot_fl_nm <- paste0("proportional_reduction_in_", my_var_name, "_", intervention_name, ".png")

png(file.path(out_fig_path, barplot_fl_nm),
    width = 9,
    height = 8,
    units = "cm",
    pointsize = 12,
    res = 300)

print(p)

dev.off()