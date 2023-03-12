# !!! do not use this function, it will not be exported !!!
# perform dbscan algorithm
dbscan <- function(data, eps, minPts) {

  data <- as.matrix(data)

  # if no column names
  if(is.null(colnames(data))) colnames(data) <- paste("val", sep="", 1:ncol(data))

  # Calculate distance matrix
  dist_matrix <- as.matrix(dist(data))

  # Run DBSCAN algorithm
  dbscan_result <- rep(0, nrow(data))
  cluster <- 0

  # Iterate over each point in the data set
  for (i in 1:nrow(data)) {

    # For each point, we find its neighbors using the distance matrix and the which function
    if (dbscan_result[i] != 0) next
    neighbors <- which(dist_matrix[i,] <= eps)

    # If the number of neighbors is less than minPts, we mark the point as noise by setting its cluster assignment to -1
    if (length(neighbors) < minPts) {
      dbscan_result[i] <- -1
      next
    }

    # We create a new cluster and mark the current point as a member of this cluster
    cluster <- cluster + 1
    dbscan_result[i] <- cluster

    # Iterate over all of the neighbors of the current point and assign them to the same cluster
    while (length(neighbors) > 0) {
      j <- neighbors[1]
      neighbors <- neighbors[-1]

      # If any of these neighbors have not yet been assigned to a cluster and have enough neighbors of their own,
      # we add them to the list of points to process
      if (dbscan_result[j] == -1) {
        dbscan_result[j] <- cluster
      }
      else if (dbscan_result[j] == 0) {
        dbscan_result[j] <- cluster
        new_neighbors <- which(dist_matrix[j,] <= eps)

        if (length(new_neighbors) >= minPts) {
          neighbors <- c(neighbors, new_neighbors)
        }
      }
    }
  }

  # Note that -1 is the noise in DBSCAN cluster
  list(data = data, clusterIds = dbscan_result, clusteredData = cbind(data, dbscan_result))
}


#' Dbscan algorithm
#'
#' Use this algorithm to identify clusters with a certain density.
#'
#' Clusters are detected if every point have a max distance of \code{eps} to other points and
#' if a cluster contains at least \code{minPts} points. The other points are noise marked with -1
#' in the result
#'
#' @param data A matrix, data.frame, tibble, with two numeric columns and at least two rows
#' @param eps A number. Max distance to other points
#' @param minPts A number of how many points should a cluster should at least have
#' @return An object of class `dbscan` with components:
#' \item{data}{original data}
#' \item{clusterIds}{A numeric vector with the cluster assignments}
#' \item{clusteredData}{A matrix with the points and its assigned cluster}
#' @examples
#' n <- 30
#' data <- cbind(xval = c(runif(n, min=0, max=3), runif(n, min=5, max=6), runif(n, min=8, max=10)),
#'               yval = c(runif(n, min=0, max=3), runif(n, min=2, max=7), runif(n, min=6, max=10)))
#'
#' db <- new_dbscan(data, 0.5, 3)
#' plot(db, titles = "regwhe5h")
#' print(db)
#' @export
new_dbscan <- function(data, eps, minPts){

  data <- as.matrix(data)
  dd <- data

  stopifnot("data must contain numeric values" = typeof(dd) == "double" | typeof(dd) == "integer" | typeof(dd) == "numeric")
  stopifnot("data is empty or does not have rows" = !is.null(nrow(dd)))
  stopifnot("data is empty or does not have columns" = !is.null(ncol(dd)))
  stopifnot("data must have two columns" = ncol(dd) == 2)
  stopifnot("data must have at least two rows" = nrow(dd) >= 2)
  stopifnot("data may contains non finite values such as Inf, NA, NULL" = all(is.finite(dd)))

  stopifnot("eps must be a number" = typeof(eps) == "integer" | typeof(eps) == "double" | length(eps) == 1)
  stopifnot("eps must be greater than 0" = eps > 0)

  stopifnot("minPts must be a number" = typeof(minPts) == "integer" | typeof(minPts) == "double" | length(minPts) == 1)
  stopifnot("minPts must be greater than 0" = minPts > 0)
  stopifnot("minPts must be smaller than number of data points" = minPts < nrow(data))

  db <- dbscan(data = data, eps = eps, minPts = minPts)
  structure(db, class = "dbscan")
}


#' Plot dbscan class
#'
#' @param obj A dbscan class, call \code{new_dbscan()} to create an dbscan object
#' @param titles A character vector with length 1. Here the titles of the plot
#' @examples
#' n <- 30
#' data <- cbind(xval = c(runif(n, min=0, max=3), runif(n, min=5, max=6), runif(n, min=8, max=10)),
#'               yval = c(runif(n, min=0, max=3), runif(n, min=2, max=7), runif(n, min=6, max=10)))
#'
#' db <- new_dbscan(data, 0.5, 3)
#' plot(db, titles = "regwhe5h")
#' @export
plot.dbscan <- function(obj, titles = "Cluster Plot", ...){

  stopifnot("titles must be a character vector" = typeof(titles) == "character")

  # +2 because we want to display the noise
  # need rainbow because we need more colors
  # noise color: #e3e3e3
  plot(obj$data, main = head(titles, 1), col = c("#e3e3e3", rainbow(max(obj$clusterIds + 1)))[obj$clusterIds + 2], pch = 20)
}


#' Print dbscan class
#'
#' @param obj A optics class, call \code{new_dbscan()} to create an dbscan object
#' @examples
#' n <- 30
#' data <- cbind(xval = c(runif(n, min=0, max=3), runif(n, min=5, max=6), runif(n, min=8, max=10)),
#'               yval = c(runif(n, min=0, max=3), runif(n, min=2, max=7), runif(n, min=6, max=10)))
#'
#' db <- new_dbscan(data, 0.5, 3)
#' print(db)
#' @export
print.dbscan <- function(obj, ...){

  cat("\nclustered data:\n")
  print(obj$clusteredData)

  cat("\nclusterIds for cluster membership:\n")
  print(obj$clusterIds)
}




################################### Example usage ###################################################

#set.seed(125)
#data <- matrix(rnorm(800), ncol = 2)
#n <- 30
#data <- cbind(qeqe = c(runif(n, min=0, max=3), runif(n, min=5, max=6), runif(n, min=8, max=10)),
#              ddd = c(runif(n, min=0, max=3), runif(n, min=2, max=7), runif(n, min=6, max=10)))
#
#db <- new_dbscan(data, 0.5, 3)
#
# Plot the results with colored clusters
#plot(db, titles = "regwhe5h")
#print(db)




