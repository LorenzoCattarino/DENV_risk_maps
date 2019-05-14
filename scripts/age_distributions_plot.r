
library("ggplot2")
        
source(file.path("R", "prepare_datasets", "grouped_data_stats.R"))
source(file.path("R", "utility_functions.R"))


# define parameters -----------------------------------------------------------


dts_out_pt <- file.path("output", "datasets")
  
dts_out_nm <- "country_age_structure_mean.csv"

out_path <- file.path("figures", "age_distribution_plots")
  
out_file_name <- "country_age_distributions"


# load data -------------------------------------------------------------------


age_distr <- read.csv(file.path("output", 
                                "datasets", 
                                "country_age_structure.csv"))


# pre processing --------------------------------------------------------------


age_distr <- age_distr[setdiff(names(age_distr), c("band_80_99", "band_85_99"))]

age_band_tgs <- grep("band", names(age_distr), value = TRUE)

age_bounds_num <- sub("^[^_]+_", "", age_band_tgs)

age_bounds_num_2 <- sub("_", "-",age_bounds_num)

names(age_distr)[names(age_distr) %in% age_band_tgs] <- age_bounds_num_2

xx <- strsplit(age_bounds_num_2, "-")
zz <- lapply(xx, as.numeric)
yy <- vapply(zz, mean, numeric(1))

mean_ages <- apply(age_distr[, age_bounds_num_2], 1, mean_grouped_data, yy)

n_c <- nrow(age_distr)

sd <- vapply(
  seq_len(n_c), 
  sd_grouped_data,
  numeric(1),
  age_dat = age_distr[, age_bounds_num_2],
  mean_vals = mean_ages) 
  
age_distr$mean <- mean_ages

age_distr$sd <- sd

age_distr <- age_distr[order(age_distr$mean), ]


# save ------------------------------------------------------------------------


write_out_csv(age_distr, dts_out_pt, dts_out_nm)


# plot ------------------------------------------------------------------------


age_distr$mean_label <- paste0("Mean = ", round(age_distr$mean, 2))

age_distr$sd_label <- paste0("SD = ", round(age_distr$sd, 2))

dir.create(out_path, FALSE, TRUE)

i <- 1
ii <- 16 

for (j in 1:13){
  
  cat("iteration =", j, "\n")
  cat("i =", i, "\n")
  cat("ii =", ii, "\n")
  
  dat_to_plot <- reshape2::melt(age_distr[i:ii,], 
                                id.vars = "country", 
                                measure.vars = age_bounds_num_2)
  
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
    geom_text(data = age_distr[i:ii,], 
              mapping = aes(x = 17, y = 0.15, label = mean_label), 
              size = 10) +
    geom_text(data = age_distr[i:ii,], 
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
