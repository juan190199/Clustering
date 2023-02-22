## Agglomerative hierarchical clustering
## Linkage functions used:
## 1. Single linkage function
## 2. Complete linkage function
## 3. Average linkage function
## 4. Wards minimum variance linkage function
## 5. Median linkage function

source("R/utils.R")

agglomerative_hierarchical_clustering <- function(data, K, linkage_fun) {
    N <- nrow(data)
    measure <- get_distance_measure(linkage_fun)

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
                if (dist < min_dist) {
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
            for (point in clusters[[id]]) {
                # print(point)
                cat("    ", point, "\n")
            }
        }
    }

    run_algorithm()
    print_()
    return(clusters)
}