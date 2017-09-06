exp_max_algorithm_boot <- function(
  i, boot_samples, 
  pxl_dataset_orig, psAbs, my_preds, 
  no_trees, min_node_size, grp_flds, niter, 
  all_wgt, pAbs_wgt,
  RF_obj_path, RF_obj_name,
  diagn_tab_path, diagn_tab_name,
  map_path, map_name, 
  sct_plt_path, adm_dataset){
  
  
  #browser()
  
  
  # ---------------------------------------- define parameters 
  
  
  pxl_dts_pt <- file.path("output", "EM_algorithm", "env_variables_foi", "boot_samples")
  
  
  # ---------------------------------------- define variables
  
  
  pxl_dts_nm <- paste0("covariates_and_foi_20km_", i, ".rds")
  
  
  # ---------------------------------------- load bootstrapped data sets 
  
  
  pxl_dts_boot <- readRDS(file.path(pxl_dts_pt, pxl_dts_nm))
  
  foi_data_boot <- boot_samples[[i]]
    

  # ---------------------------------------- get output name 
  
  
  a <- RF_obj_name[i]
  b <- diagn_tab_name[i]
  cc <- map_path[i]  
  ee <- map_name[i] 
  ff <- sct_plt_path[i]
  
  
  # ---------------------------------------- pre process the bootstrapped foi data set
  
  
  foi_data_boot[foi_data_boot$type == "pseudoAbsence", "o_j"] <- psAbs
  
  foi_data_boot$new_weight <- all_wgt
  
  foi_data_boot[foi_data_boot$type == "pseudoAbsence", "new_weight"] <- pAbs_wgt
  
  names(foi_data_boot)[names(foi_data_boot) == "FOI"] <- "o_j"
  
  names(foi_data_boot)[names(foi_data_boot) == "ADM_0"] <- grp_flds[1]
  names(foi_data_boot)[names(foi_data_boot) == "ADM_1"] <- grp_flds[2]
  
  
  # ---------------------------------------- pre process the square data set
  
  
  names(pxl_dts_boot)[names(pxl_dts_boot) == "ADM_0"] <- grp_flds[1]
  names(pxl_dts_boot)[names(pxl_dts_boot) == "ADM_1"] <- grp_flds[2]
  
  pxl_dts_grp <- pxl_dts_boot %>% group_by_(.dots = grp_flds) 
  
  aa <- pxl_dts_grp %>% summarise(pop_sqr_sum = sum(population))
  
  pxl_dts_boot <- left_join(pxl_dts_boot, aa)
  
  pxl_dts_boot$pop_weight <- pxl_dts_boot$population / pxl_dts_boot$pop_sqr_sum
  
  pxl_dts_boot$new_weight <- all_wgt
  
  pxl_dts_boot[pxl_dts_boot$type == "pseudoAbsence", "new_weight"] <- pAbs_wgt
  
  
  # ---------------------------------------- attach original data to square dataset
  
  
  pxl_dts_boot <- inner_join(pxl_dts_boot, foi_data_boot[, c(grp_flds, "o_j")])
  
  
  # ---------------------------------------- run the EM 
  
  
  exp_max_algorithm(niter = niter, 
                    orig_dataset = foi_data_boot, 
                    pxl_dataset = pxl_dts_boot,
                    pxl_dataset_full = pxl_dataset_orig,
                    l_f = psAbs,
                    my_predictors = my_preds, 
                    grp_flds = grp_flds, 
                    RF_obj_path = RF_obj_path,
                    RF_obj_name = a,
                    diagn_tab_path = diagn_tab_path, 
                    diagn_tab_name = b,
                    map_path = cc, 
                    map_name = ee,
                    sct_plt_path = ff,
                    adm_dataset = adm_dataset)
  
}
