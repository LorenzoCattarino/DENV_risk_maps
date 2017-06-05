map_burden_measures <- function(i, a, b, c, ...) {
  map_burden_admin_scale (y_var = a[i], 
                          map_file = a[i],
                          map_title = b[i],
                          do.log = c[i],
                          ...)
}

replace_missing <- function(x, b){
  x[is.na(x)] <- b
  x
}
