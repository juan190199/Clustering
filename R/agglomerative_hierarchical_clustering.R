## Agglomerative hierarchical clustering
## Linkage functions used:
## 1. Single linkage function
## 2. Complete linkage function
## 3. Average linkage function
## 4. Wards minimum variance linkage function
## 5. Median linkage function

# ToDo: Check if all linkage functions are working
# ToDo: Create sanity checks
# ToDo: Create test functions

source("R/utils.R")

agglomerative_hierarchical_clustering <- function(data, K, linkage_fun) {
    N <- nrow(data)
    measure <- get_distance_measure(linkage_fun)
    order <- c()
    labels <- c()

    init_clusters <- function() {
        clusters <- list()
        for(i in seq(N)){
            clusters[[i]] <- list(data[i, ])
        }
        return(clusters)
    }
    clusters <- init_clusters()

    find_closest_clusters <- function() {
        min_dist <- Inf
        closest_clusters <- c(NA, NA)

        clusters_ids <- 1:length(clusters)

        for (i in 1:(length(clusters_ids) - 1)) {
            for (j in (i+1):length(clusters_ids)) {
                dist <- measure(clusters[[clusters_ids[i]]], clusters[[clusters_ids[j]]])
                if (dist <= min_dist) {
                    min_dist <- dist
                    closest_clusters <- c(clusters_ids[i], clusters_ids[j])
                }
            }
        }
        return(closest_clusters)
    }

    merge_and_form_new_clusters <- function(ci_id, cj_id) {
        new_clusters <- list()

        for (cluster_id in 1:length(clusters)) {
            if ((cluster_id == ci_id) | (cluster_id == cj_id)) {
                next
            }
            new_clusters[[length(new_clusters) + 1]] <- clusters[[cluster_id]]
        }

        new_clusters[[length(new_clusters) + 1]] <- c(clusters[[ci_id]], clusters[[cj_id]])
        return(new_clusters)
    }

    run_algorithm <- function() {
        while (length(clusters) > K) {
            closest_clusters <- find_closest_clusters()
            clusters <<- merge_and_form_new_clusters(closest_clusters[1], closest_clusters[2])
        }
    }

    print_ <- function() {
        for (id in 1:length(clusters)) {
            cat("Cluster: ", id, "\n")
            idx <- sapply(clusters[[id]], function(x) which(apply(data, 1, function(y) all(y == x))))
            order <<- c(order, c(idx))
            for (i in 1:length(clusters[[id]])) {
                labels[idx[[i]]] <<- id
                cat(idx[[i]], "\t", clusters[[id]][[i]], "\n")
            }
        }
    }

    run_algorithm()
    print_()

    output <- list(order = order, clusters = clusters, method = linkage_fun, labels = labels)
    return(output)
}

# library(datasets)
# data(iris)
#
# # Standardize data
# iris_std <- data.matrix(scale(iris[, 1:4]))
#
# result <- agglomerative_hierarchical_clustering(iris_std, 3, "complete")
#
# # Perform PCA on the iris dataset
# iris_pca <- prcomp(iris[, 1:4], scale = TRUE, center = TRUE)
# iris_pca_2d <- iris_pca$x[, 1:2]
#
# # Plot the iris data in two dimensions using the first two principal components
# plot(iris_pca_2d, col = result$labels, pch = 19, xlab = "PC1", ylab = "PC2")
# legend("bottomright", legend = unique(result$labels), col = 1:length(unique(result$labels)), pch = 19)
#
# # Plot the iris data in two dimensions using the first two principal components
# plot(iris_pca_2d, col = iris$Species, pch = 19, xlab = "PC1", ylab = "PC2")
# legend("bottomright", legend = unique(iris$Species), col = 1:length(unique(iris$Species)), pch = 19)
#
# iris_hclust <- hclust(dist(iris_std[, 1:4]))
# iris_clusters <- cutree(iris_hclust, k = 3)
#
# # Plot the iris data in two dimensions using the first two principal components, colored by cluster
# plot(iris_pca_2d, col = iris_clusters, pch = 19, xlab = "PC1", ylab = "PC2")
# legend("bottomright", legend = unique(iris_clusters), col = 1:length(unique(iris_clusters)), pch = 19)
