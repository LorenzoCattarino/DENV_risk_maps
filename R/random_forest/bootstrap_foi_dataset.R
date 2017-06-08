do_boostrap <- function(dataset){
  idx <- unname(split(seq_len(nrow(dataset)), dataset$cell))
  pick <- sample(idx, size = length(idx), replace = TRUE)
  dataset[unlist(pick), ]
}