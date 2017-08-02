# load packages
library(ggplot2)
library(grid)

# load functions 
source(file.path("R", "burden_and_interventions", "get_age_band_bounds.r"))
source(file.path("R", "burden_and_interventions", "wrapper_to_get_R0.r"))
source(file.path("R", "burden_and_interventions", "calculate_probabilities_and_R0.r"))
source(file.path("R", "burden_and_interventions", "calculate_infection_probability_and_number.r"))
source(file.path("R", "burden_and_interventions", "calculate_average_infect_probab.r"))
source(file.path("R", "burden_and_interventions", "calculate_R0.r"))


# ---------------------------------------- load data 


All_FOI_estimates <- read.table(
  file.path("output", "foi", "All_FOI_estimates_linear.txt"), 
  header = TRUE, 
  sep = ",", 
  stringsAsFactors = FALSE)

country_age_struc <- read.csv(
  file.path("output", 
            "datasets", 
            "country_age_structure.csv"))

adm_1_env_vars <- read.csv(
  file.path("output", 
            "env_variables", 
            "All_adm1_env_var.csv"))


# ---------------------------------------- define parameters


m_flds <- c("ID_0", "ID_1")

interm_outs <- c("R0")

base_info <- c("data_id", "ID_0", "ID_1", "FOI", "variance", "population")

my_phis <- c(1, 1, 1, 1)


# ---------------------------------------- extract info from age structure 


# Get names of age band columns
age_band_tgs <- grep("band", names(country_age_struc), value = TRUE)

# Get age band bounds
age_band_bnds <- get_age_band_bounds(age_band_tgs)

age_band_L_bounds <- age_band_bnds[, 1]

age_band_U_bounds <- age_band_bnds[, 2] + 1


# ---------------------------------------- pre process age structure


zero_age_str_countries <- apply(country_age_struc[, age_band_tgs], 1, sum) == 0

country_age_struc <- country_age_struc[!zero_age_str_countries, ]


# ---------------------------------------- pre process FOI dataset

# Remove missing values 
All_FOI_estimates_2 <- All_FOI_estimates[!is.na(All_FOI_estimates$FOI), ]


# ---------------------------------------- preprocess admin dataset


adm_1_env_vars <- adm_1_env_vars[!duplicated(adm_1_env_vars[, m_flds]), ]


# ---------------------------------------- merge population data


All_FOI_estimates_3 <- merge(
  All_FOI_estimates_2, 
  adm_1_env_vars[, c(m_flds, "population")], 
  by = m_flds, 
  all.y = FALSE)


# ---------------------------------------- filter out data points with NA age structure data


dd <- setNames(data.frame(country_age_struc[, m_flds[1]]), nm = m_flds[1])

All_FOI_estimates_4 <- merge(
  All_FOI_estimates_3, 
  dd, 
  by = m_flds[1], 
  all.y = FALSE)


# ---------------------------------------- convert to matrix


All_FOI_estimates_5 <- as.matrix(All_FOI_estimates_4[, base_info])

rownames(All_FOI_estimates_5) <- NULL


# ---------------------------------------- calculate R0


R_0 <- apply(
  All_FOI_estimates_5, 
  1, 
  wrapper_to_get_R0, 
  age_data = country_age_struc, 
  age_band_lower_bounds = age_band_L_bounds, 
  age_band_upper_bounds = age_band_U_bounds, 
  age_band_tags = age_band_tgs,
  vec_phis = my_phis,
  info_1 = interm_outs)


# ---------------------------------------- attach back base info


All_R_0_estimates <- data.frame(
  type = All_FOI_estimates_4$type,
  ISO = All_FOI_estimates_4$ISO,
  longitude = All_FOI_estimates_4$longitude,
  latitude = All_FOI_estimates_4$latitude,
  All_FOI_estimates_5, 
  R_0)


# ---------------------------------------- save output


write.table(All_R_0_estimates, 
            file.path("output", "R_0", "All_R_0_estimates.csv"), 
            row.names = FALSE, 
            sep = ",")


# ---------------------------------------- plot 


All_R_0_estimates <- All_R_0_estimates[order(All_R_0_estimates$FOI), ]

All_R_0_estimates$ID_point <- seq_len(nrow(All_R_0_estimates))

png(file.path("figures", "reprod_number_plot.png"), 
    width = 20, 
    height = 14, 
    units = "in", 
    pointsize = 12,
    bg = "white", 
    res = 300)

lambda_plot <- ggplot(All_R_0_estimates, aes(x = ID_point, y = FOI, colour = type)) +
               geom_point(size = 0.8) +
               scale_x_continuous(name = "Country code", breaks = seq_len(nrow(All_R_0_estimates)), 
                                  expand = c(0.002, 0)) +
               scale_y_continuous(name = "FOI") +
               theme(axis.text.x = element_text(size = 5, angle = 90, hjust = 0.5, vjust = 0.5),
                     panel.grid.minor = element_blank())

R_0_plot <- ggplot(All_R_0_estimates, aes(x = ID_point, y = R_0, colour = type)) +
            geom_point(size = 0.8) +
            scale_x_continuous(name = "Country code", breaks = seq_len(nrow(All_R_0_estimates)), 
                               expand = c(0.002, 0)) +
            scale_y_continuous(name = "R_0") +
            theme(axis.text.x = element_text(size = 5, angle = 90, hjust = 0.5, vjust = 0.5),
                  panel.grid.minor = element_blank())

grid.draw(rbind(ggplotGrob(lambda_plot), ggplotGrob(R_0_plot), size = "first"))
                   
dev.off()
