library(dplyr)



get_distance_measure <- function(linkage_fun) {
  if (linkage_fun == "single") {
    return(single_link)
  } else if (linkage_fun == "complete") {
    return(complete_link)
  } else if (linkage_fun == "average") {
    return(average_link)
  } else if (linkage_fun == "wards") {
    return(wards_minimum_variance_link)
  } else if (linkage_fun == "median") {
    return(median_link)
  }
}
