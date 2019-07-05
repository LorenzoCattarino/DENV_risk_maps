
library(dplyr)
library(ggplot2)

source(file.path("R", "prepare_datasets", "grouped_data_stats.R"))


# define parameters -----------------------------------------------------------


out_path <- file.path("figures", "age_distribution_plots")

out_file_name <- "country_age_distributions"


# load data -------------------------------------------------------------------


age_struct_data <- read.csv(file.path("output", 
                                "datasets", 
                                "country_age_structure.csv"))


# preprocessing ---------------------------------------------------------------


old_col_names <- grep("band", names(age_struct_data), value = TRUE)

new_col_names <- edit_age_band_tags(age_struct_data)

age_struct_data <- age_struct_data %>% 
  rename_at(vars(old_col_names), ~ new_col_names)

age_struct_data <- age_struct_data[order(age_struct_data$mean_age), ]


# plot ------------------------------------------------------------------------


age_struct_data$mean_label <- paste0("Mean = ", round(age_struct_data$mean_age, 2))

age_struct_data$sd_label <- paste0("SD = ", round(age_struct_data$sd_age, 2))

dir.create(out_path, FALSE, TRUE)

i <- 1
ii <- 16 

for (j in 1:13){
  
  cat("iteration =", j, "\n")
  cat("i =", i, "\n")
  cat("ii =", ii, "\n")
  
  dat_to_plot <- reshape2::melt(age_struct_data[i:ii,], 
                                id.vars = "country", 
                                measure.vars = new_col_names)
  
  dat_to_plot$country <- droplevels(dat_to_plot$country)
  
  dat_to_plot$country <- factor(dat_to_plot$country, 
                                levels = unique(dat_to_plot$country))
  
  my_name <- sprintf("%s_page_%d%s", out_file_name, j, ".png")
  
  png(file.path(out_path, my_name),
      width = 30,
      height = 20,
      units = "in",
      pointsize = 12,
      res = 200)
  
  p <- ggplot(dat_to_plot, aes(x = variable, y = value)) +
    geom_bar(stat = "identity") +
    geom_text(data = age_struct_data[i:ii,], 
              mapping = aes(x = 17, y = 0.15, label = mean_label), 
              size = 10) +
    geom_text(data = age_struct_data[i:ii,], 
              mapping = aes(x = 17, y = 0.1, label = sd_label), 
              size = 10) +
    facet_wrap(~country, ncol = 4, nrow = 4) +
    scale_x_discrete("Age bands") +
    scale_y_continuous("") +
    theme(strip.text = element_text(size = 20),
          axis.text.x = element_text(size = 9),
          axis.text.y = element_text(size = 20),
          axis.title.x = element_text(size = 20, margin = margin(t = 20)))
  
  print(p)
  
  dev.off()
  
  if (j == 12){
    
    i <- i + 16
    ii <- ii + 8 
    
  } else {
    
    i <- i + 16
    ii <- ii + 16 
    
  }
  
}
