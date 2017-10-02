rep_num <- function(i, custom_adm0_codes, custom_adm1_codes){
  num_to_rep <- custom_adm0_codes[i]
  n_to_rep <- length(custom_adm1_codes[[i]])
  rep(num_to_rep, n_to_rep)
}

map_ID_1 <- function(c_name, shp_file){
  
  #print(c_name)
  lyb <- subset(shp_file, NAME_0 == c_name)
  lyb_fort <- fortify(lyb)
  centroids_df <- as.data.frame(coordinates(lyb))
  names(centroids_df) <- c("Longitude", "Latitude")
  centroids_df$id <- lyb@data$ID_1
  
  ggplot() + 
    geom_path(data=lyb_fort, 
              aes(x=long, y=lat, group=group),
              colour = "black") +
    geom_text(data=centroids_df, aes(label = id, x = Longitude, y = Latitude))
  
}
