get_sero <- function(FOI, age){
  1 - (exp(-4 * FOI * age))
}
