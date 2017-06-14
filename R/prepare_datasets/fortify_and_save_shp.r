fortify_and_save <- function(shp_fl, out_path, out_name){
  a <- fortify(shp_fl)
  write_out_rds(a, out_path, out_name)
}
