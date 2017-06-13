grid_and_boot <- function(a, b){
  
  xx <- grid_up(a, b, rnd_dist = FALSE)
  
  do_boostrap(xx)

}
