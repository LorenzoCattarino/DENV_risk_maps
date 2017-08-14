dwnl.gadm.shp <- function(x)
{
  country_code <- x
  URL <- sprintf("http://biogeo.ucdavis.edu/data/gadm2.8/shp/%s_%s%s", country_code, "adm_shp", ".zip")
  temp <- tempfile(fileext = ".zip")
  download.file(URL, temp)
  unzip(temp, exdir = file.path("data", "shapefiles", paste(country_code, "adm_shp", sep="_")))
}
