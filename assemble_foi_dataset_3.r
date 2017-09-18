# For each original foi estimate, calculates the corresponding R0 (only one assumption)

# load packages
library(ggplot2)
library(grid)

# load functions 
source(file.path("R", "burden_and_interventions", "functions_to_calculate_R0_and_burden.r"))


# ---------------------------------------- define parameters


m_flds <- c("ID_0", "ID_1")

base_info <- c("type", "ISO", "longitude", "latitude", "data_id", "ID_0", "ID_1", "FOI", "variance", "population")

var <- "R_0"

my_phis <- c(1, 1, 1, 1)

prob_fun <- list("calculate_primary_infection_prob",
                 "calculate_secondary_infection_prob",
                 "calculate_tertiary_infection_prob",
                 "calculate_quaternary_infection_prob")


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


All_FOI_estimates_3 <- merge(
  All_FOI_estimates_2, 
  country_age_struc[, m_flds[1], drop = FALSE], 
  by = m_flds[1], 
  all.y = FALSE)


# ---------------------------------------- calculate R0


n <- nrow(All_FOI_estimates_3)

R_0 <- vapply(seq_len(n), function(i, foi_data, age_struct, age_band_tags, age_band_lower_bounds, age_band_upper_bounds, vec_phis, prob_fun){
  m_j <- age_struct[age_struct$ID_0 == foi_data[i, "ID_0"], age_band_tags]
  FOI <- foi_data[i, "FOI"]
  calculate_R0(
    FOI = FOI, 
    N = 1, 
    n_j = m_j, 
    age_band_lower_bounds = age_band_lower_bounds, 
    age_band_upper_bounds = age_band_upper_bounds,
    vec_phis = vec_phis, 
    prob_fun = prob_fun)}, 
  numeric(1),
  foi_data = All_FOI_estimates_3, 
  age_struct = country_age_struc, 
  age_band_lower_bounds = age_band_L_bounds, 
  age_band_upper_bounds = age_band_U_bounds, 
  age_band_tags = age_band_tgs,
  vec_phis = my_phis,
  prob_fun = prob_fun)


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
