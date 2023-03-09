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
  n_i <- length(ci)
  n_j <- length(cj)
  n <- n_i + n_j

  # Calculate the centroids of ci and cj
  centroid_i <- rep(0, length(ci[[1]])) # initialize a vector of 0s
  for (k in 1:length(ci)) {
    centroid_i <- centroid_i + ci[[k]]
  }
  centroid_i <- centroid_i / n_i

  centroid_j <- rep(0, length(cj[[1]])) # initialize a vector of 0s
  for (k in 1:length(cj)) {
    centroid_j <- centroid_j + cj[[k]]
  }
  centroid_j <- centroid_j / n_j

  # Calculate the sum of squares within ci and cj
  ssq_i <- sum(sapply(ci, function(x) sum((x - centroid_i)^2)))
  ssq_j <- sum(sapply(cj, function(x) sum((x - centroid_j)^2)))

  # Calculate the sum of squares after merging ci and cj
  merged <- c(ci, cj)
  centroid_merged <- rep(0, length(merged[[1]])) # initialize a vector of 0s
  for (k in 1:length(merged)) {
    centroid_merged <- centroid_merged + merged[[k]]
  }
  centroid_merged <- centroid_merged / n

  ssq_merged <- sum(sapply(merged, function(x) sum((x - centroid_merged)^2)))

  # Calculate the increase in sum of squares due to merging
  increase <- ssq_merged - ssq_i - ssq_j

  # Return the square root of the increase divided by the number of points
  return(sqrt(increase / n))
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
