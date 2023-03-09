dbscan <- function(data, eps, minPts) {

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

new_dbscan <- function(data, eps, minPts){

  stopifnot("data must be a matrix" = class(data)[1] == "matrix")
  stopifnot("data must be a matirx with numeric values" = typeof(data) == "integer" | typeof(data) == "double" | typeof(data) == "numeric")
  stopifnot("eps must be greater than 0" = eps > 0)
  stopifnot("eps must be a number" = typeof(eps) == "integer" | typeof(eps) == "double")
  stopifnot("minPts must be greater than 0" = minPts > 0)
  stopifnot("minPts must be a number" = typeof(minPts) == "integer" | typeof(minPts) == "double")

  db <- dbscan(data = data, eps = eps, minPts = minPts)
  structure(db, class = "dbscan")
}

plot.dbscan <- function(obj){
  # +2 because we want to display the noise
  # need rainbow because we need more colors
  # noise color: #e3e3e3
  plot(obj$data, col = c("#e3e3e3", rainbow(max(obj$clusterIds + 1)))[obj$clusterIds + 2], pch = 20)
}

print.dbscan <- function(obj){
  str(db)
}



# Generate sample data
#set.seed(125)
#data <- matrix(rnorm(800), ncol = 2)
# n <- 110
# data <- cbind(c(runif(n, min=0, max=3), runif(n, min=5, max=6), runif(n, min=8, max=10)),
#               c(runif(n, min=0, max=3), runif(n, min=2, max=7), runif(n, min=6, max=10)))
#
# db <- new_dbscan(data, 0.4, 5)
#
# # Plot the results with colored clusters
# plot(db)
# print(db)




