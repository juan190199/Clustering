library(dplyr)
source("R/utils/utils.R")

############################
##    Linkage functions   ##
############################

single_link <- function(ci, cj, dist_method = "euclidean") {
  return(min(sapply(ci, function(vi) {
    sapply(cj, function(vj) {
      dist_func(vi, vj, type = dist_method)
    })
  })))
}

complete_link <- function(ci, cj, dist_method = "euclidean") {
  return(max(sapply(ci, function(vi) {
    sapply(cj, function(vj) {
      dist_func(vi, vj, type = dist_method)
    })
  })))
}

average_link <- function(ci, cj, dist_method = "euclidean") {
  distances <- sapply(ci, function(vi) {
    sapply(cj, function(vj) {
      dist_func(vi, vj, type = dist_method)
    })
  })
  return(mean(distances))
}

median_link <- function(ci, cj, dist_method = "euclidean") {
  distances <- sapply(ci, function(vi) {
    sapply(cj, function(vj) {
      dist_func(vi, vj, type = dist_method)
    })
  })
  return(median(distances))
}

wards_minimum_variance_link <- function(ci, cj, dist_method = "euclidean") {
  ni <- length(ci)
  nj <- length(cj)
  centroid_i <- apply(ci, 2, mean)
  centroid_j <- apply(cj, 2, mean)
  return(sqrt((ni * nj) / (ni + nj)) * dist_func(centroid_i, centroid_j, type = dist_method))
}

centroid_link <- function(ci, cj, dist_method = "euclidean") {
  centroid_i <- apply(ci, 2, mean)
  centroid_j <- apply(cj, 2, mean)
  return(dist_func(centroid_i, centroid_j, type = dist_method))
}

mcquitty_link <- function(ci, cj, dist_method = "euclidean") {
  ni <- length(ci)
  nj <- length(cj)
  return(sqrt((ni * nj) / (ni + nj)) * sum(sapply(ci, function(vi) {
    sapply(cj, function(vj) {
      dist_func(vi, vj, type = dist_method)
    })
  })) / (ni * nj))
}


get_distance_measure <- function(linkage_fun) {
  if (linkage_fun == "single") {
    return(single_link)
  } else if (linkage_fun == "complete") {
    return(complete_link)
  } else if (linkage_fun == "average") {
    return(average_link)
  } else if (linkage_fun == "median") {
    return(median_link)
  } else if (linkage_fun == "wards") {
    return(wards_minimum_variance_link)
  } else if (linkage_fun == "centroid") {
    return(centroid_link)
  } else if (linkage_fun == "mcquitty") {
    return(mcquitty_link)
  } else {
    stop("Linkage function does not exist. Possible linkage functions are: single, complete, average, wards, median, centroid, mcquitty")
  }
}
