# For each original foi estimate, calculates the corresponding R0 (only one assumption)

# load packages
library(ggplot2)
library(grid)

# load functions 
source(file.path("R", "prepare_datasets", "functions_for_calculating_R0.r"))


# ---------------------------------------- define parameters


m_flds <- c("ID_0", "ID_1")

base_info <- c("type", "ISO", "longitude", "latitude", "data_id", "ID_0", "ID_1", "FOI", "variance", "population")

gamma_1 <- 0.45
rho <- 0.85
gamma_3 <- 0.15

phi_combs <- list(
  c(1, 1, 0, 0),
  c(1, 1, 1, 1),
  calculate_infectiousness_wgts_for_sym_asym_assumption(gamma_1, rho, gamma_3))

prob_fun <- list("calculate_primary_infection_prob",
                 "calculate_secondary_infection_prob",
                 "calculate_tertiary_infection_prob",
                 "calculate_quaternary_infection_prob")


# ---------------------------------------- define variables


comb_no <- length(phi_combs)

var <- paste0("R0_", seq_len(comb_no))


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


# ---------------------------------------- calculate R0 for all 3 assumptions


R_0 <- vapply(
  phi_combs,
  wrapper_to_multi_factor_R0,
  numeric(nrow(All_FOI_estimates_3)),
  foi_data = All_FOI_estimates_3, 
  age_struct = country_age_struc, 
  age_band_tags = age_band_tgs, 
  age_band_lower_bounds = age_band_L_bounds, 
  age_band_upper_bounds = age_band_U_bounds, 
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

R_0_plot <- ggplot(All_R_0_estimates, aes(x = ID_point, y = R0_2, colour = type)) +
            geom_point(size = 0.8) +
            scale_x_continuous(name = "Country code", breaks = seq_len(nrow(All_R_0_estimates)), 
                               expand = c(0.002, 0)) +
            scale_y_continuous(name = "R_0") +
            theme(axis.text.x = element_text(size = 5, angle = 90, hjust = 0.5, vjust = 0.5),
                  panel.grid.minor = element_blank())

grid.draw(rbind(ggplotGrob(lambda_plot), ggplotGrob(R_0_plot), size = "first"))
                   
dev.off()
