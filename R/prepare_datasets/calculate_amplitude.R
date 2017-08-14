calc_amplitude <- function(x, var_1, var_2, a, b) {
  2 * sqrt((x[, var_1])^2 + (x[, var_2])^2) / (a * b)
}
