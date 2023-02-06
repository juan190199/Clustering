## Hierarchical clustering. We will use Euclidean distance.

## Clusterin methods:
##
## 1. Ward's minimum variance
## 2. Single linkage
## 3. Complete linkage
## 4. Average linkage
## 5. Median method
## 6. Centroid method

# Agglomerative Hierarchical Clustering in R

# Helper function to calculate euclidean distance between two points
euclidean_distance <- function(x, y) {
  sqrt(sum((x - y)^2))
}

# Main function to perform Agglomerative Hierarchical Clustering
agglomerative_hierarchical_clustering <- function(X, linkage_fun) {
  n <- nrow(X)  # Number of datapoints
  clusters <- list(1:n)
  cluster_distances <- matrix(0, nrow=n, ncol=n)
  while (length(clusters) > 1) {
    # Calculate pairwise distances between clusters
    for (i in 1:(length(clusters) - 1)) {
      for (j in (i + 1):length(clusters)) {
        cluster_distances[i, j] <- linkage_fun(clusters[[i]], clusters[[j]])
      }
    }
  }
}


