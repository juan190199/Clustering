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

wards_minimum_variance_link <- function(ci, cj) {
  n <- length(ci) + length(cj)
  centroid_ci <- colMeans(ci)
  centroid_cj <- colMeans(cj)
  centroid_new_cluster <- (n / (length(ci) + length(cj))) * (centroid_ci + centroid_cj)
  return(distance_r(centroid_ci, centroid_new_cluster) + distance_r(centroid_cj, centroid_new_cluster))
}

median_link <- function(ci, cj) {
  n <- length(ci) + length(cj)
  centroid_ci <- colMeans(ci)
  centroid_cj <- colMeans(cj)
  return(distance(centroid_ci, centroid_cj))
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