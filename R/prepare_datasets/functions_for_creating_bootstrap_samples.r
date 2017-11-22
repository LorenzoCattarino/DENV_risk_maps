grid_and_boot <- function(i, a, b){
  
  xx <- grid_up(a, b, rnd_dist = FALSE)
  
  do_boostrap(xx)

}

do_boostrap <- function(dataset){
  #browser()
  idx <- unname(split(seq_len(nrow(dataset)), dataset$cell))
  pick <- sample(idx, size = length(idx), replace = TRUE)
  dataset[unlist(pick), ]
}

attach_unique_id <- function(i, b_sam) {
 
  x <- b_sam[[i]]
  cbind(unique_id = seq_len(nrow(x)), x)

}
