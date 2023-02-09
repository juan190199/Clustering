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

set.seed(101)

# Helper function to calculate euclidean distance between two matrices
euclidean_distance_matrix <- function(mat1, mat2)
{
  n1 <- nrow(mat1)
  n2 <- nrow(mat2)
  dists <- matrix(0, n1, n2)
  for (i in 1:n1) {
    diffs <- mat2 - mat1[i, ]
    dists[i, ] <- sqrt(rowSums(diffs^2))
  }
  return(dists)
}

# Main function to perform Agglomerative Hierarchical Clustering
agglomerative_hierarchical_clustering <- function(X, linkage_fun) {
  n <- nrow(X)  # Number of datapoints
  cluster_distances <- matrix(0, nrow=n, ncol=n)
  diag(cluster_distances) <- rep(1e10, n)  # Set diagonal to high value to avoid self-pairing

  # Initialize each data point as a single-point cluster
  clusters <- as.list(1:n)

  # Define function for linkage method
  linkage_fun <- switch(
    linkage_fun,
    "single" = function (cluster1, cluster2) {
      dists <- euclidean_distance_matrix(cluster1, cluster2)
      return(min(dists))
    },
    "complete" = function (cluster1, cluster2) {
      dists <- euclidean_distance_matrix(cluster1, cluster2)
      return(max(dists))
    },
    "average" = function (cluster1, cluster2) {
      dists <- euclidean_distance_matrix(cluster1, cluster2)
      return(mean(dists))
    },
    "median" = function (cluster1, cluster2) {
      dists <- euclidean_distance_matrix(cluster1, cluster2)
      return(median(dists))
    },
    "centroid" = function (cluster1, cluster2) {
      centroid1 <- apply(X[cluster1, ], 2, mean)
      centroid2 <- apply(X[cluster2, ], 2, mean)
      return(euclidean_distance_matrix(centroid1, centroid2))
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
    for (i in 1:(length(clusters) - 1))
    {
      for (j in (i + 1):length(clusters))
      {
        cluster_distances[i, j] <- linkage_fun(X[clusters[[i]], , drop=FALSE], X[clusters[[j]], , drop=FALSE])
      }
    }

    # Find the two closest clusters to merge
    min_distance <- min(cluster_distances[cluster_distances > 0])
    close_cluster_indices <- which(cluster_distances == min_distance, arr.ind=TRUE)
    close_cluster1 <- close_cluster_indices[[1]]
    close_cluster2 <- close_cluster_indices[[2]]

    # Merge the two closest clusters
    clusters[[close_cluster1]] <- c(clusters[[close_cluster1]], clusters[[close_cluster2]])
    clusters <- clusters[-close_cluster2]
    # cluster_distances <- cluster_distances[-close_cluster2, -close_cluster2]

    # For each cluster set the distance between elements of the same cluster to inf
    for (i in 1:(length(clusters[[close_cluster1]])))
    {
      for (j in 1:(length(clusters[[close_cluster2]])))
      {
        cluster_distances[close_cluster1[i], close_cluster2[j]] <- 1e10
      }
    }
  }

  return(clusters[[1]])
}

X <- matrix(rnorm(100), ncol=2)
result <- agglomerative_hierarchical_clustering(X, "single")