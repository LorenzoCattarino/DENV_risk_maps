sub_n_sample <- function(i, a){
  x <- ifelse(nrow(i) >= a, a, nrow(i))
  ids <- sample(seq_len(nrow(i)), x)
  i[ids,]
}
