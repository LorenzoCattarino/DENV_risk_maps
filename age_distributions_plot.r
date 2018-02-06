library("ggplot2")
        
source(file.path("R", "prepare_datasets", "functions_for_calculating_R0.r"))

out_path <- file.path("figures", "age_distribution_plots")
  
out_file_name <- "country_age_distributions"

country_age_struc <- read.csv(
  file.path("output", 
            "datasets", 
            "country_age_structure.csv"))

country_age_struc <- country_age_struc[setdiff(names(country_age_struc), c("band_80_99", "band_85_99"))]

age_band_tgs <- grep("band", names(country_age_struc), value = TRUE)

age_bounds_num <- sub("^[^_]+_", "", age_band_tgs)

age_bounds_num_2 <- sub("_", "-",age_bounds_num)

names(country_age_struc) [names(country_age_struc) %in% age_band_tgs] <- age_bounds_num_2

xx <- strsplit(age_bounds_num_2, "-")
zz <- lapply(xx, as.numeric)
yy <- vapply(zz, mean, numeric(1))

mean_ages <- apply(country_age_struc[, age_bounds_num_2], 1, function(i) sum(i * yy))

n_c <- nrow(country_age_struc)

sd_fun <- function(i, age_dat, mean_vals) (sqrt(sum((yy^2) * age_dat[i,]) - mean_vals[i]^2))

sd <- vapply(
  seq_len(n_c), 
  sd_fun,
  numeric(1),
  age_dat = country_age_struc[, age_bounds_num_2],
  mean_vals = mean_ages) 
  
country_age_struc$mean <- mean_ages

country_age_struc$sd <- sd

country_age_struc$mean_label <- paste0("Mean = ", round(country_age_struc$mean, 2))

country_age_struc$sd_label <- paste0("SD = ", round(country_age_struc$sd, 2))

country_age_struc <- country_age_struc[order(country_age_struc$mean), ]

dir.create(out_path, FALSE, TRUE)

i <- 1
ii <- 16 

for (j in 1:13){
  
  cat("iteration =", j, "\n")
  cat("i =", i, "\n")
  cat("ii =", ii, "\n")
  
  dat_to_plot <- reshape2::melt(country_age_struc[i:ii,], id.vars = "country", measure.vars = age_bounds_num_2)
  
  dat_to_plot$country <- droplevels(dat_to_plot$country)
  
  dat_to_plot$country <- factor(dat_to_plot$country, levels = unique(dat_to_plot$country))
  
  my_name <- sprintf("%s_page_%d%s", out_file_name, j, ".png")
  
  png(file.path(out_path, my_name),
      width = 30, # original: 7
      height = 20, # original: 3
      units = "in",
      pointsize = 12,
      res = 200)
  
  p <- ggplot(dat_to_plot, aes(x = variable, y = value)) +
    geom_bar(stat = "identity") +
    geom_text(data = country_age_struc[i:ii,], aes(x = 17, y = 0.15, label = mean_label), size = 10) +
    geom_text(data = country_age_struc[i:ii,], aes(x = 17, y = 0.1, label = sd_label), size = 10) +
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
