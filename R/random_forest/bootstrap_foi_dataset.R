do_boostrap <- function(dataset){
  #browser()
  idx <- unname(split(seq_len(nrow(dataset)), dataset$cell))
  pick <- sample(idx, size = length(idx), replace = TRUE)
  dataset[unlist(pick), ]
}