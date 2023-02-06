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
  cluster_distances <- matrix(0, nrow=n, ncol=n)
  diag(cluster_distances) <- rep(1e10, n)  # Set diagonal to high value to avoid self-pairing

  # Initialize each data point as a single-point cluster
  clusters <- list(1:n)

  # Define function for linkage method
  linkage_fun <- switch(
    linkage_method,
    "single" = function (cluster1, cluster2) {
      dists <- apply(expand.grid(cluster1, cluster2), 1, function(x) euclidean_distance(X[x[1], ], X[x[2], ]))
      return(min(dists))
    },
    "complete" = function (cluster1, cluster2) {
      dists <- apply(expand.grid(cluster1, cluster2), 1, function(x) euclidean_distance(X[x[1], ], X[x[2], ]))
      return(max(dists))
    },
    "average" = function (cluster1, cluster2) {
      dists <- apply(expand.grid(cluster1, cluster2), 1, function(x) euclidean_distance(X[x[1], ], X[x[2], ]))
      return(mean(dists))
    },
    "median" = function (cluster1, cluster2) {
      dists <- apply(expand.grid(cluster1, cluster2), 1, function(x) euclidean_distance(X[x[1], ], X[x[2], ]))
      return(median(dists))
    },
    "centroid" = function (cluster1, cluster2) {
      centroid1 <- apply(X[cluster1, ], 2, mean)
      centroid2 <- apply(X[cluster2, ], 2, mean)
      return(euclidean_distance(centroid1, centroid2))
    },
    "ward" = function (cluster1, cluster2) {
      sse1 <- sum(sapply(cluster1, function(x) {
        sum((X[x, ] - colMeans(X[cluster1, ]))^2)
      }))
      sse2 <- sum(sapply(cluster2, function(x) {
        sum((X[x, ] - colMeans(X[cluster2, ]))^2)
      }))
      n1 <- length(cluster1)
      n2 <- length(cluster2)

      return((n1 * sse1 + n2 * sse2) / (n1 + n2))
    }
  )

  while (length(clusters) > 1) {
    # Calculate pairwise distances between clusters
    for (i in 1:(length(clusters) - 1)) {
      for (j in (i + 1):length(clusters)) {
        cluster_distances[i, j] <- linkage_fun(clusters[[i]], clusters[[j]])
      }
    }
  }

}


