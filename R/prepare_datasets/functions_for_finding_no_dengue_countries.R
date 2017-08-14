get_admin1_names <- function(x)
{
  if(length(x)>1)
  {
    out <- x[2] 
  }else{
    out <- NA 
  }  
  out
}

check_country_name_in_shp <- function (x, world_shp_admin_1, Attr) 
{
  country_name <- x[Attr]
  #cat("country name = ", country_name, "\n")
  
  logic <- country_name %in% world_shp_admin_1@data[,Attr]
  logic
}
