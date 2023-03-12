#library(tidyverse)


# !!! do not use this function, it will not be exported !!!
# perform kMeans algorithm
kMeans <- function(data, k = 1, maxIterations = 100, setInitialCentroids = NULL) {

  data <- as.matrix(data)

  # choose start values for the clusers manually
  init <- ifelse(is.null(setInitialCentroids), TRUE, FALSE)

  # keep start value of k
  init_k <- k
  k <- as.integer(k)

  clusterIds <- NULL
  centroids <- setInitialCentroids

  if(init){
    # init the start values of the cluster membership randomly
    clusterIds <- sample.int(k, nrow(data), replace = TRUE)
  }

  # loop over max iteration
  for (i in 1:maxIterations) {

    # Achtung es kann sein, dass centroids zusammenfallen und sich damit deren Anzahl
    # verringert im vergleich zu gesetzten k, also setzte k auf die neue Anzahl damit
    # index out of bounce verhindert wird und gebe eine Warnung aus
    num_cl <- length(unique(clusterIds))
    if(num_cl < k && init){
      k <- num_cl
    }

    if(init){

      # if no column names
      if(is.null(colnames(data))) colnames(data) <- paste("val", sep="", 1:ncol(data))

      # calculate the center points of the clusters
      centroids <- as_tibble(data)
      centroids <- centroids %>% add_column(cid = clusterIds)
      centroids <- centroids %>% group_by(cid) %>% summarise(across(everything(), mean))
      centroids <- as.matrix(centroids %>% select(-cid))
    }

    # calculate the distance to the cluster centers
    distances <- sapply(1:k, function(j) rowSums((data - centroids[j,])^2))

    # assign the data points to its nearest cluster
    newClusterIds <- apply(distances, 1, which.min)

    # check if cluster membership has changed
    if (all(clusterIds == newClusterIds) & !is.null(clusterIds)) break

    # The new values for the cluster membership
    clusterIds <- newClusterIds
    init <- TRUE
  }

  if(init_k > k){
    warning("The maximum number of centroids ist less than k ==> k was set to high or not an integer")
  }

  # return some cluster info
  list(data = data, clusterIds = clusterIds, clusteredData = cbind(data, clusterIds), centroids = centroids)
}


# !!! do not use this function, it will not be exported !!!
# perform kMeans++ algorithm
kMeans_pp <- function(data, k, maxIterations = 100) {

  X <- data

  # choose the first cluster center randomly
  centroids <- rbind(X[sample(nrow(X), 1), ])

  # repeat until k cluster centers were found
  while (nrow(centroids) < k) {

    # calculate the distance of every point to its nearest cluster center
    distances <- apply(X, 1, function(x) min(sqrt(rowSums((centroids - x)^2))))

    # choose the next cluster center proportional to the distance
    next_centroid <- rbind(X[sample.int(nrow(X), size = 1, prob = distances), ])

    # add the next cluster center to the list of cluster centers
    centroids <- rbind(centroids, next_centroid)
  }

  # execute kMeans with our pre-calculated cluster centers (centroids)
  kMeans(X, k, maxIterations = maxIterations, setInitialCentroids = centroids)
}


#' kMeans algorithm
#'
#' Use this algorithm to identify a given number of clusters.
#'
#' kMeans clustering is a method of vector quantization, originally from signal
#' processing, that aims to partition n observations into k clusters in which
#' each observation belongs to the cluster with the nearest mean (cluster
#' centers or cluster centroid), serving as a prototype of the cluster.
#'
#' @param data A matrix, data.frame, tibble, with two numeric colums and at least two rows
#' @param k A number of how many clusters should be detected
#' @param maxIterations A number. kMeans converges fast set \code{maxIterations} to 10-100 is mostly ok
#' @param setInitialCentroids A matrix with x,y columns. Set start values ie points, if not than
#' the algorithm does that randomly
#' @param type A character string. What kind of algorithm here the standart kMeans and kMeans++ algorithm
#' @return An object of class `kMeans` with components:
#' \item{data}{original data}
#' \item{clusterIds}{A numeric vector with the cluster assignments}
#' \item{clusteredData}{A matrix with the points and its assigned cluster}
#' \item{centroids}{A numeric vector with the center points of the clusters}
#' @examples
#' n <- 50
#' data <- cbind(wrw = c(runif(n, min=0, max=3), runif(n, min=5, max=6), runif(n, min=8, max=10)),
#'               yys = c(runif(n, min=0, max=3), runif(n, min=2, max=7), runif(n, min=6, max=10)))
#'
#' km <- new_kMeans(data, 3, 20, type = "kMeans++")
#'
#'
#' #visualize
#' plot(km, titles = "ewgfsgw")
#' print(km)
#' @export
new_kMeans <- function(data, k = 1, maxIterations = 100, setInitialCentroids = NULL, type = "kMeans"){

  # type =
  # kMeans
  # kMeans++

  data <- as.matrix(data)
  dd <- data

  stopifnot("type must be a character string" = typeof(type) == "character" | length(type) == 1)
  stopifnot("type must be a character string" = length(type) == 1)

  if(type == "kMeans++" && !is.null(setInitialCentroids)){
    stop("For kMeans++ algorithm it is not possible to set setInitialCentroids to a non NULL value")
  }

  stopifnot("Wrong type of algorithm ==> set it to kMeans or kMeans++" = type == "kMeans" | type == "kMeans++")

  stopifnot("data must contain numeric values" = typeof(dd) == "double" | typeof(dd) == "integer" | typeof(dd) == "numeric")
  stopifnot("data is empty or does not have rows" = !is.null(nrow(dd)))
  stopifnot("data is empty or does not have columns" = !is.null(ncol(dd)))
  stopifnot("data must have two columns" = ncol(dd) == 2)
  stopifnot("data must have at least two rows" = nrow(dd) >= 2)
  stopifnot("data may contains non finite values such as Inf, NA, NULL" = all(is.finite(dd)))

  stopifnot("k must be a number" = typeof(k) == "integer" | typeof(k) == "double")
  stopifnot("k must be greater than 0" = k > 0)

  stopifnot("maxIterations must be a number" = typeof(maxIterations) == "integer" | typeof(maxIterations) == "double")
  stopifnot("maxIterations must be greater than 0" = maxIterations > 0)

  stopifnot("setInitialCentroids must be a matrix or NULL" = is.null(setInitialCentroids) |  class(setInitialCentroids)[1] == "matrix")
  stopifnot("setInitialCentroids must be a matirx with numeric values" = typeof(setInitialCentroids) == "integer" |
              typeof(setInitialCentroids) == "double" | typeof(setInitialCentroids) == "numeric" | is.null(setInitialCentroids))
  stopifnot("setInitialCentroids is empty or does not have rows" = !is.null(nrow(setInitialCentroids)) | is.null(setInitialCentroids))
  stopifnot("setInitialCentroids is empty or does not have columns" = !is.null(ncol(setInitialCentroids))| is.null(setInitialCentroids))
  stopifnot("number of rows from setInitialCentroids must be equal to k" = k == nrow(setInitialCentroids) | is.null(setInitialCentroids))
  stopifnot("setInitialCentroids may contains non finite values such as Inf, NA, NULL" = all(is.finite(setInitialCentroids)) | is.null(setInitialCentroids))

  if(type == "kMeans"){
    km <- kMeans(data = data, k = k, maxIterations = maxIterations, setInitialCentroids = setInitialCentroids)
    return(structure(km, class = "kMeans"))
  }

  else if(type == "kMeans++"){
    km_pp = kMeans_pp(data = data, k = k, maxIterations = maxIterations)
    return(structure(km_pp, class = "kMeans"))
  }
}


#' Plot kMeans class
#'
#' @param obj A kMeans class, call \code{new_kMeans()} to create an kMeans object
#' @param titles A character vector with length 1. Here the titles of the plot
#' @examples
#' n <- 50
#' data <- cbind(wrw = c(runif(n, min=0, max=3), runif(n, min=5, max=6), runif(n, min=8, max=10)),
#'               yys = c(runif(n, min=0, max=3), runif(n, min=2, max=7), runif(n, min=6, max=10)))
#'
#' km <- new_kMeans(data, 3, 20, type = "kMeans++")
#'
#'
#' #visualize
#' plot(km, titles = "ewgfsgw")
#' @export
plot.kMeans <- function(obj, titles = "Cluster Plot", ...){

  stopifnot("titles must be a character vector" = typeof(titles) == "character")

  plot(obj$data, main = head(titles, 1), col = rainbow(max(obj$clusterIds))[obj$clusterIds], pch = 20)
  points(obj$centroids, col = "black", lwd = 4)
}


#' Print kMeans class
#'
#' @param obj A kMeans class, call \code{new_kMeans()} to create an kMeans object
#' @examples
#' n <- 50
#' data <- cbind(wrw = c(runif(n, min=0, max=3), runif(n, min=5, max=6), runif(n, min=8, max=10)),
#'               yys = c(runif(n, min=0, max=3), runif(n, min=2, max=7), runif(n, min=6, max=10)))
#'
#' km <- new_kMeans(data, 3, 20, type = "kMeans++")
#'
#' print(km)
#' @export
print.kMeans <- function(obj, ...){

  cat("center points:\n")
  print(obj$centroids)
  cat("\ncluster membership data:\n")
  print(obj$clusteredData)

}




################################### Example usage ###################################################

#n <- 50
#data <- cbind(wrw = c(runif(n, min=0, max=3), runif(n, min=5, max=6), runif(n, min=8, max=10)),
#              yys = c(runif(n, min=0, max=3), runif(n, min=2, max=7), runif(n, min=6, max=10)))
#
#km <- new_kMeans(data, 3, 20, type = "kMeans++")


# visualisize
#plot(km, titles = "ewgfsgw")
#print(km)



