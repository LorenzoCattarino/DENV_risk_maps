# Calculates, for each model fit and R0-Wolbachia effect assumption combinations
# R0 values and burden, for different foi value (squares).

# Takes the mean and confidence intervals, across models,  
# of different measured variables (foi, R0, burden), for each tile. 


# ----------------------------------------


options(didehpc.cluster = "fi--didemrchnb")

my_resources <- c(
  file.path("R", "utility_functions.r"),
  file.path("R", "burden_and_interventions", "wrapper_to_multi_factor_R0_and_burden.r"),
  file.path("R", "burden_and_interventions", "wrapper_to_replicate_R0_and_burden.r"),  
  file.path("R", "burden_and_interventions", "wrapper_to_R0_and_burden.r"),
  file.path("R", "burden_and_interventions", "functions_to_calculate_R0_and_burden.r"))

my_pkgs <- c("data.table", "dplyr", "reshape2", "ggplot2")

context::context_log_start()
ctx <- context::context_save(path = "context",
                             packages = my_pkgs,
                             sources = my_resources)

context::context_load(ctx)
context::parallel_cluster_start(8, ctx)

CLUSTER <- TRUE


# ---------------------------------------- define parameters


model_tp <- "boot_model_20km_cw_2" 

var_names <- c("R0", "I_num", "C_num", "I_inc", "C_inc")

prob_fun <- list("calculate_primary_infection_prob",
                 "calculate_secondary_infection_prob",
                 "calculate_tertiary_infection_prob",
                 "calculate_quaternary_infection_prob")

sf_vals <- c(1, 0.7, 0.3)
phi_set_id <- c(1, 4)
phi_set_id_tag <- "phi_set_id"
gamma_1 <- 0.45
rho <- 0.85
gamma_3 <- 0.15

v1 <- c(1, 1, 0, 0) # Up to 2 infections
v2 <- c(1, 1, 1, 0) # Up to 3 infections
v3 <- c(1, 1, 1, 1) # Up to 4 infections 

out_path <- file.path("output", "predictions_world", model_tp)


# ---------------------------------------- load data


all_sqr_mean_foi <- readRDS(
  file.path(
    "output", 
    "predictions_world",
    model_tp,
    "all_squares_mean_foi_0_1667_deg.rds"))

age_struct <- read.csv(
  file.path("output", 
            "datasets",
            "country_age_structure.csv"), 
  header = TRUE) 


# ---------------------------------------- 


age_struct$age_id <- seq_len(nrow(age_struct))

names(age_struct)[names(age_struct) == "ID_0"] <- "ADM_0"


# ---------------------------------------- keep onle the FOI for which there is age data available


all_sqr_mean_foi <- inner_join(
  age_struct[, c("age_id", "country", "ADM_0")],
  all_sqr_mean_foi, 
  by = "ADM_0")


# ---------------------------------------- create table of scenarios 


v4 <- calculate_infectiousness_wgts_for_sym_asym_assumption(gamma_1, rho, gamma_3)
phi_combs <- setNames(data.frame(seq_len(4), rbind(v1, v2, v3, v4)),
                      nm = c(phi_set_id_tag, "phi1", "phi2", "phi3", "phi4"))

fct_c <- setNames(expand.grid(phi_set_id, sf_vals),
                  nm = c(phi_set_id_tag, "scaling_factor"))

fct_c <- cbind(id = seq_len(nrow(fct_c)), fct_c)

fct_c_2 <- left_join(fct_c, phi_combs, by = phi_set_id_tag)
write.csv(fct_c_2, 
          "output/predictions_world/boot_model_20km_cw/scenario_table.csv", 
          row.names = FALSE)

fctr_combs <- df_to_list(fct_c_2, use_names = TRUE)


# ----------------------------------------


age_band_tgs <- grep("band", names(age_struct), value = TRUE)
age_band_bnds <- get_age_band_bounds(age_band_tgs)
age_band_L_bounds <- age_band_bnds[, 1]
age_band_U_bounds <- age_band_bnds[, 2] + 1


# ---------------------------------------- create FOI -> Inf and FOI -> C lookup tables 


max_FOI <- max(all_sqr_mean_foi$mean_pred)

FOI_values <- seq(0, 0.05, by = 0.0002) 

if(!file.exists(file.path(out_path, "FOI_to_I_lookup_tables.rds"))){
  
  Infection_values <- loop(seq_len(nrow(age_struct)), 
                           wrapper_to_lookup,
                           age_struct = age_struct, 
                           tags = age_band_tgs, 
                           FOI_values = FOI_values, 
                           my_fun = calculate_infections,
                           prob_fun = prob_fun,
                           age_band_lower_bounds = age_band_L_bounds,
                           age_band_upper_bounds = age_band_U_bounds,
                           parallel = TRUE)
  
  FOI_to_Inf_list <- lapply(Infection_values, function(i) cbind(x = FOI_values, y = i))
  
  saveRDS(FOI_to_Inf_list, file.path(out_path, "FOI_to_I_lookup_tables.rds"))
  
} else {
  
  FOI_to_Inf_list <- readRDS(file.path(out_path, "FOI_to_I_lookup_tables.rds"))
  
}

if(!file.exists(file.path(out_path, "FOI_to_C_lookup_tables.rds"))){
  
  Case_values <- loop(seq_len(nrow(age_struct)), 
                      wrapper_to_lookup,
                      age_struct = age_struct, 
                      tags = age_band_tgs, 
                      FOI_values = FOI_values, 
                      my_fun = calculate_cases,
                      prob_fun = prob_fun,
                      age_band_lower_bounds = age_band_L_bounds,
                      age_band_upper_bounds = age_band_U_bounds,
                      rho = rho, 
                      gamma_1 = gamma_1, 
                      gamma_3 = gamma_3,
                      parallel = TRUE)
  
  FOI_to_C_list <- lapply(Case_values, function(i) cbind(x = FOI_values, y = i))
  
  saveRDS(FOI_to_C_list, file.path(out_path, "FOI_to_C_lookup_tables.rds"))
  
} else{
  
  FOI_to_C_list <- readRDS(file.path(out_path, "FOI_to_C_lookup_tables.rds"))
  
}

# ------------------------------------------ submit jobs 


if (CLUSTER) {
  
  config <- didehpc::didehpc_config(template = "12and16Core")
  obj <- didehpc::queue_didehpc(ctx, config = config)
  
}

if (CLUSTER) {
  
  R0_and_burden <- queuer::qlapply(
    fctr_combs,
    wrapper_to_multi_factor_R0_and_burden,
    obj,
    foi_data = all_sqr_mean_foi, 
    age_data = age_struct,
    age_band_tags = age_band_tgs,
    age_band_lower_bounds = age_band_L_bounds,
    age_band_upper_bounds = age_band_U_bounds,
    parallel_2 = TRUE,    
    var_names = var_names, 
    FOI_values = FOI_values,
    FOI_to_Inf_list = FOI_to_Inf_list,
    FOI_to_C_list = FOI_to_C_list,
    prob_fun = prob_fun)
  
} else {
  
  R0_and_burden <- loop(
    fctr_combs,
    wrapper_to_multi_factor_R0_and_burden,
    foi_data = all_sqr_mean_foi, 
    age_data = age_struct,
    age_band_tags = age_band_tgs,
    age_band_lower_bounds = age_band_L_bounds,
    age_band_upper_bounds = age_band_U_bounds,
    parallel_2 = TRUE,    
    var_names = var_names, 
    FOI_values = FOI_values,
    FOI_to_Inf_list = FOI_to_Inf_list,
    FOI_to_C_list = FOI_to_C_list,
    prob_fun = prob_fun,
    parallel = FALSE)
  
}

context::parallel_cluster_stop()


# ---------------------------------------- combine all scenario results 


all_bur_scenarios <- R0_and_burden$results()

all_bur_scenarios <- do.call("cbind", all_bur_scenarios)

out <- cbind(all_sqr_mean_foi, all_bur_scenarios)


# ---------------------------------------- post processing 


zero_logic <- out$mean_pred == 0

out_mz <- out[!zero_logic, ] 

out_mz$p9 <- 100 * (1 - exp(-36 * out_mz$mean_pred))

names(out_mz)[names(out_mz) == "mean_pred"] <- "FOI"

write_out_rds(out_mz,
              out_path, 
              "R0_and_burden_all_combs.rds")


# ---------------------------------------- summarize burden by country 


n_combs <- nrow(fct_c_2)

var_to_sum <- c("population",
                paste0("I_num_", seq_len(n_combs)),
                paste0("C_num_", seq_len(n_combs)))

by_country <- out_mz %>% group_by(country)

inf_cas_sums <- by_country %>% summarise_each("sum", one_of(var_to_sum))

num_Inf_and_C_by_c <- as.data.frame(inf_cas_sums)

write.csv(num_Inf_and_C_by_c, 
          file.path(out_path, "total_infec_and_cases_by_country.csv"),
          row.names = FALSE)


# ---------------------------------------- plot number and incidence of infections and cases, for each scenario  


summed_vars <- colSums(num_Inf_and_C_by_c[, var_to_sum])

resh_res <- setNames(data.frame(matrix(summed_vars[2:length(var_to_sum)], ncol = 2)),
                     nm = c("Infections", "Cases"))

resh_res$Incidence_of_infections <- (resh_res$Infections / summed_vars[1]) * 1000

resh_res$Incidence_of_cases <- (resh_res$Cases / summed_vars[1]) * 1000

summary_table <- cbind(fct_c_2, resh_res)

summary_table$scaling_factor <- factor(summary_table$scaling_factor, 
                                       levels = c(1,0.7,0.3), 
                                       labels = c(1,0.7,0.3))

phi_factor_levels <- c("Up to 2 infections", "Up to 4 infections (sym twice as infectious as asym)")

summary_table$phi_set_id <- factor(summary_table$phi_set_id, 
                                   levels = c(1, 4), 
                                   labels = phi_factor_levels)

summary_table_long <- melt(summary_table, 
                           id.vars = c("phi_set_id", "scaling_factor"), 
                           measure.vars = c("Infections", "Cases", "Incidence_of_infections", "Incidence_of_cases"))

two_dts <- list(summary_table_long[1:12,], summary_table_long[13:nrow(summary_table_long),])

fl_nms <- c("Numbers_of_infections_and_cases_plot.png", "Incidence_of_infections_and_cases_plot.png")
ylabs <- c("Total numbers", "Incidence") 

lapply(seq_along(two_dts), function(i){
  
  ggplot(two_dts[[i]], aes(scaling_factor, value)) + 
    geom_bar(aes(fill = variable), stat = "identity", position = "dodge") +
    scale_fill_manual(values = c("red", "blue"),
                      labels = c("Infections", "Cases"),
                      guide = guide_legend(title = NULL, 
                                           keywidth = 2, 
                                           keyheight = 2)) +
    facet_grid(. ~ phi_set_id) +
    xlab("Wolbachia induced R0 reduction") +
    ylab(ylabs[i])
  
  dir.create(file.path("figures", "predictions_world", model_tp), FALSE, TRUE)
  
  ggsave(file.path("figures", "predictions_world", model_tp, fl_nms[i]),
         width = 12, 
         height = 4, 
         units = "in")
  
})
