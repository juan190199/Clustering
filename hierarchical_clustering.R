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




