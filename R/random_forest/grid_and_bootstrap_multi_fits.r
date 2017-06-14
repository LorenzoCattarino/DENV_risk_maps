grid_and_boot_multi <- function(i, a, b){
  
  xx <- grid_up(a, b, rnd_dist = FALSE)
  
  do_boostrap(xx)

}
