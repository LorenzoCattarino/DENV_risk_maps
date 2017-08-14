calculate_total_cases_by_country <- function(x) {
  
  # Group df by country
  by_country <- x %>% group_by(country)
  
  # Summarize burden measures by country
  out <- by_country %>% summarise_each("sum", total_pop = population, total_infections = number_of_infections, total_cases = number_of_cases)
  
  as.data.frame(out)
}  

calculate_incidence_of_total_cases_by_country <- function(x) {
  
  xx <- lapply(x[c("total_infections", "total_cases")], calculate_incidence_of_infections, x$total_pop)
  names(xx) <- c("incidence_of_total_infections", "incidence_of_total_cases")
  as.data.frame(xx)
}
