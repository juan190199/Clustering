source("utils.R")

agglomerative_hierarchical_clustering <- function(data, K, linkage_fun) {
    N <- nrow(data)
    measure <- get_distance_measure(linkage_fun)

    init_clusters <- function() {
        return(lapply(1:N, function(i) data[i, ]))
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


}