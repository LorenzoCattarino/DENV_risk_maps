# For each original foi estimate, calculates the corresponding R0 (only one assumption)

# load packages
library(ggplot2)
library(grid)

# load functions 
source(file.path("R", "burden_and_interventions", "get_age_band_bounds.r"))
source(file.path("R", "burden_and_interventions", "wrapper_to_get_R0.r"))
source(file.path("R", "burden_and_interventions", "calculate_infection_probability_and_number.r"))
source(file.path("R", "burden_and_interventions", "calculate_average_infect_probab.r"))
source(file.path("R", "burden_and_interventions", "calculate_R0.r"))


# ---------------------------------------- define parameters


m_flds <- c("ID_0", "ID_1")

base_info <- c("type", "ISO", "longitude", "latitude", "data_id", "ID_0", "ID_1", "FOI", "variance", "population")

var <- "R_0"

my_phis <- c(1, 1, 1, 1)


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


# ---------------------------------------- extract info from age structure 


# Get names of age band columns
age_band_tgs <- grep("band", names(country_age_struc), value = TRUE)

# Get age band bounds
age_band_bnds <- get_age_band_bounds(age_band_tgs)

age_band_L_bounds <- age_band_bnds[, 1]

age_band_U_bounds <- age_band_bnds[, 2] + 1


# ---------------------------------------- preprocess admin dataset


adm_1_env_vars <- adm_1_env_vars[!duplicated(adm_1_env_vars[, m_flds]), ]


# ---------------------------------------- merge population data


All_FOI_estimates_2 <- merge(
  All_FOI_estimates, 
  adm_1_env_vars[, c(m_flds, "population")], 
  by = m_flds, 
  all.y = FALSE)


# ---------------------------------------- filter out data points with NA age structure data


dd <- setNames(data.frame(country_age_struc[, m_flds[1]]), nm = m_flds[1])

All_FOI_estimates_3 <- merge(
  All_FOI_estimates_2, 
  dd, 
  by = m_flds[1], 
  all.y = FALSE)


# ---------------------------------------- create look up table for age structure


info_age <- country_age_struc[, c("ID_0", age_band_tgs)]
test_id <- match(All_FOI_estimates_3[, "ID_0"], info_age$ID_0)
look_up <- info_age[test_id, ]


# ---------------------------------------- calculate R0


n <- nrow(All_FOI_estimates_3)

R_0 <- vapply(
  seq_len(n),
  wrapper_to_get_R0, 
  numeric(1),
  df = All_FOI_estimates_3, 
  age_data = look_up, 
  age_band_lower_bounds = age_band_L_bounds, 
  age_band_upper_bounds = age_band_U_bounds, 
  age_band_tags = age_band_tgs,
  vec_phis = my_phis)


# ---------------------------------------- attach base info


All_R_0_estimates <- setNames(cbind(All_FOI_estimates_3[, base_info],
                                    R_0),
                              nm = c(base_info, var))


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
