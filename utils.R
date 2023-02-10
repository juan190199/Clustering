library(dplyr)

distance <- function(p, q) {
  return(sqrt(sum((p - q)^2)))
}

single_link <- function(ci, cj) {
  return(min(sapply(ci, function(vi) {
    sapply(cj, function(vj) {
      distance(vi, vj)
    })
  })))
}

complete_link <- function(ci, cj) {
  return(max(sapply(ci, function(vi) {
    sapply(cj, function(vj) {
      distance(vi, vj)
    })
  })))
}

average_link <- function(ci, cj) {
  distances <- sapply(ci, function(vi) {
    sapply(cj, function(vj) {
      distance(vi, vj)
    })
  })
  return(mean(distances))
}


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
