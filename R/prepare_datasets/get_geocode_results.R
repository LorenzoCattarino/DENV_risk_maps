get_geocode_results <- function(geocode_run)
{
  number_of_matches <- length(geocode_run$results)

  all_results <- data.frame(type=rep(0,number_of_matches,
                                   formatted_address=rep(0,number_of_matches), 
                                   lat=rep(0,number_of_matches), lng=rep(0,number_of_matches)))

  all_results$type <- sapply(geocode_run$results, function(x){x$types[[1]]})
  all_results$formatted_address <- sapply(geocode_run$results, function(x){x$formatted_address})
  all_results$lat <- sapply(geocode_run$results, function(x){x$geometry$location$lat})
  all_results$lng <- sapply(geocode_run$results, function(x){x$geometry$location$lng})
  
  all_results$point_1_x <- sapply(geocode_run$results, function(x){x$geometry$viewport$northeast$lng})
  all_results$point_1_y <- sapply(geocode_run$results, function(x){x$geometry$viewport$northeast$lat})
  all_results$point_2_x <- sapply(geocode_run$results, function(x){x$geometry$viewport$northeast$lng})
  all_results$point_2_y <- sapply(geocode_run$results, function(x){x$geometry$viewport$southwest$lat})
  all_results$point_3_x <- sapply(geocode_run$results, function(x){x$geometry$viewport$southwest$lng})
  all_results$point_3_y <- sapply(geocode_run$results, function(x){x$geometry$viewport$southwest$lat})
  all_results$point_4_x <- sapply(geocode_run$results, function(x){x$geometry$viewport$southwest$lng})
  all_results$point_4_y <- sapply(geocode_run$results, function(x){x$geometry$viewport$northeast$lat})
  
  all_results
}
