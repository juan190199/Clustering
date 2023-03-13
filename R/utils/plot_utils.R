#' Hi
plot_cluster <- function(cluster, title, colors, legend){
  UseMethod("plot_cluster")
}

plot_cluster.spectral <- function(cluster, title, colors, legend){
  NextMethod()
}

plot_cluster.hierarchical <- function(cluster, title=NULL, legend = unique(cluster$labels)){
  if(is.null(title)){
    if(attr(cluster, "class")[1] == "spectral"){
      title <- "Spectral Clustering (hierarchichal)"
    } else{
      title <- "Hierarchichal Clustering"
    }
  } else{
    stopifnot("Title has to be a character vector"= is.vector(title) && is.character(title))
  }

  num_clusters <- length(unique(cluster$labels))
  stopifnot("Legend needs to have the same number of labels as our cluster"=
              num_clusters == length(legend))

  par(mar=c(5, 4, 4, 4), xpd = TRUE)

  # Plot by two dimensions using the first two principal components
  plot(cluster$data, main=title(title), col = cluster$labels, pch = 19, xlab = "PC1", ylab = "PC2", cex=1)
  legend("bottomright", inset = c(-0.17, 0), legend = legend, col = seq(num_clusters), pch = 19, cex=1)
}

plot_cluster.kmedoid <- function(cluster, title=NULL, legend = unique(cluster$medoids)){
  if(is.null(title)){
    if(attr(cluster, "class")[1] == "spectral"){
      title <- "Spectral Clustering (kmedoid)"
    } else{
      title <- "KMedoid Clustering"
    }
  } else{
    stopifnot("Title has to be a character vector"= is.vector(title) && is.character(title))
  }

  num_clusters <- length(unique(cluster$medoids))
  stopifnot("Legend needs to have the same number of labels as our cluster"=
              num_clusters == length(legend))

  par(mar=c(5, 4, 4, 4), xpd = TRUE)

  # Plot by two dimensions using the first two principal components
  medoids <- cluster$medoids
  plot(cluster$data[-medoids, ], main=title(title), col = cluster$clusters[-medoids], pch = 1, xlab = "PC1", ylab = "PC2", cex=1)
  points(cluster$data[medoids, ], cex=1.5, pch=17, col = cluster$clusters[medoids])
  legend("bottomright", inset = c(-0.17, 0), legend = c(legend, "medoids"), col = seq(num_clusters+1), pch = c(rep(1, num_clusters), 17), cex=1)
}

#' Plot kMeans class
#'
#' @param cluster A kMeans class, call \code{new_kMeans()} to create an kMeans object
#' @param title A character vector with length 1. Contains the title of the plot.
#' @examples
#' n <- 50
#' data <- cbind(wrw = c(runif(n, min=0, max=3), runif(n, min=5, max=6), runif(n, min=8, max=10)),
#'               yys = c(runif(n, min=0, max=3), runif(n, min=2, max=7), runif(n, min=6, max=10)))
#'
#' km <- new_kMeans(data, 3, 20, type = "kMeans++")
#'
#'
#' #visualize
#' plot(km, titles = "ewgfsgw")
#' @export
plot_cluster.kMeans <- function(cluster, title=NULL){
  if(is.null(title)){
    if(attr(cluster, "class")[1] == "spectral"){
      title <- "Spectral Clustering (kmeans)"
    } else{
      title <- "KMeans Clustering"
    }
  } else{
    stopifnot("Title has to be a character vector"= is.vector(title) && is.character(title))
  }

  num_clusters <- nrow(cluster$centroids)

  par(mar=c(5, 4, 4, 6), xpd = TRUE)
  plot(cluster$data, main = head(title, 1), col = rainbow(num_clusters)[cluster$clusterIds], pch = 1)
  points(cluster$centroids, col = "black", pch=19, cex=1.5)
  legend("bottomright", inset = c(-0.2, 0), legend = c("centroids"), col = "black", pch = 19)
}

plot_cluster.optics <- function(){

}

plot_cluster.dbscan <- function(){

}
