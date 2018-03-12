calculate_sd <- function(i, values, indices){
  #browser()
  a <- values[i,]
  b <- indices[i,]
  sd(a[b==1])
}
