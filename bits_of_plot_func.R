ID.exp <- unique(df_to_plot$exp_ID)

file.path("figures", 
          "dengue_dataset", 
          "sensitivity_analysis",
          output_folder,
          paste("exp", ID.exp, sep = "_"))
          
full_file_name <- paste0(file_name, "_exp_", ID.exp, ".jpg")

