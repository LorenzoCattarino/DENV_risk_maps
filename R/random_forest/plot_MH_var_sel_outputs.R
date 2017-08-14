plot_MH_var_sel_outputs <- function(data_to_plot, run_id, exp_id){
  
  if(!is.data.frame(data_to_plot)){
    
    data_to_plot <- as.data.frame(data_to_plot)
  
  }
     
  file_tag <- sprintf("visual_diagnostics_%s%s", paste("run", run_id, sep = "_"), ".pdf")
  
  dir.create(file.path("figures", 
                       "dengue_dataset", 
                       "variable_selection", 
                       "metropolis_hastings",
                       paste("exp", exp_id, sep = "_")),
             FALSE, TRUE)
  
  # plot and save
  p1 <- ggplot(
    data_to_plot, aes(iter, cur.OF_after)) +
    geom_line() +
    scale_x_continuous("Iterations")
  
  p2 <- ggplot(
    data_to_plot, aes(cur.OF_after)) +
    geom_histogram() + 
    scale_y_continuous("Frequency")
  
  p3 <- grid.arrange(p1, p2, top = paste("Run", run_id, sep = " "))
  
  ggsave(file.path("figures", 
                   "dengue_dataset", 
                   "variable_selection", 
                   "metropolis_hastings", 
                   paste("exp", exp_id, sep = "_"),
                   file_tag), 
         plot = p3)
  
}