grid_and_boot <- function(i, a, b){
  
  xx <- grid_up(a, b, rnd_dist = FALSE)
  
  do_boostrap(xx)

}
