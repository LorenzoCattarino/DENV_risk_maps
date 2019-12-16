create_sequential_palette <- function(n){
  
  #pal_fun <- colorRampPalette(RColorBrewer::brewer.pal(n = 9, name = "YlGnBu"))
  #pal_fun(100)
  
  rev(sequential_hcl(n, palette = "BluYl"))
  
}