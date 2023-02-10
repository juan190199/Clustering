source("utils.R")

agglomerative_hierarchical_clustering <- function(data, K, linkage_fun) {
    N <- nrow(data)
    measure <- get_distance_measure(linkage_fun)

    init_clusters <- function() {
        return(lapply(1:N, function(i) data[i, ]))
    }
    clusters <- init_clusters()

}