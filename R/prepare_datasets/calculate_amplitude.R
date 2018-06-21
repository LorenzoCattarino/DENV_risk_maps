calc_amplitude <- function(x, var_1, var_2, year.f, year.i, ppyear) {
  ntimes <- (year.f - year.i + 1) * ppyear
  2 * sqrt((x[, var_1])^2 + (x[, var_2])^2) / ntimes
}
