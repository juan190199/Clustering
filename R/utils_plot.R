#' Hi
plot_cluster <- function(cluster, title, legend, ...){
  UseMethod("plot_cluster")
}

plot_cluster.spectral <- function(cluster, title, legend, ...){
  NextMethod()
}

plot_cluster.hierarchical <- function(cluster, title=NULL, legend = unique(cluster$labels)){
  if(is.null(title)){
    if(attr(cluster, "class")[1] == "spectral"){
      title <- "Spectral Clustering (hierarchical)"
    } else{
      title <- "Hierarchical Clustering"
    }
  } else{
    stopifnot("Title has to be a character vector"= is.vector(title) && is.character(title))
  }

  num_clusters <- length(unique(cluster$labels))
  stopifnot("Legend needs to have the same number of labels as our cluster"=
              num_clusters == length(legend))

  par(mar=c(5, 4, 4, 4), xpd = TRUE)

  # Plot by two dimensions using the first two principal components
  plot(cluster$data, main=head(title, 1), col = cluster$labels, pch = 19, cex=1)
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
  plot(cluster$data[-medoids, ], main=head(title, 1), col = cluster$clusters[-medoids], pch = 1, cex=1)
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

#' Plot optics class
#'
#' @param obj A optics class, call \code{new_optics()} to create an optics object
#' @param orderline A logical to determine if the data points should be connected with lines in reachability order
#' @param type A character to decide which plot should be displayed: `reachability_cluster` show both plots, `reachability` show reachability plot
#' `cluster` show cluster plot
#' @param titles A character vector with length 2. Here the titles of the plots for reachability and cluster
#' @param cluster_colored A logical. Should the data be colored by the cluster assignments
#' @examples
#' n <- 100
#' data <- cbind(xval = c(runif(n*2, min=0, max=3), runif(n, min=5, max=6), runif(n/11, min=8, max=10)),
#'               yval = c(runif(n*2, min=0, max=3), runif(n, min=2, max=7), runif(n/11, min=6, max=10)))
#'
#' res <- new_optics(data = data, minPts = 7, num_cluster = 3)
#' plot(res)
#' print(res)
#' @export
plot_cluster.optics <- function(cluster, orderline = FALSE, type = "reachability_cluster", titles = c("Cluster plot", "Reachability plot"), cluster_colored = TRUE, ...){
    # type =
    # reachability_cluster, show both plots
    # reachability, show reachability plot
    # cluster, show cluster plot

    stopifnot("type must be a character" = typeof(type) == "character")
    stopifnot("type must be a character string" = length(type) == 1)

    stopifnot("Wrong type please choose: reachability_cluster or reachability or cluster" = type == "reachability_cluster" |
                type == "reachability" | type == "cluster")

    stopifnot("titles must be a character vector" = typeof(titles) == "character")
    stopifnot("orderline must be a logical type" = typeof(orderline) == "logical")
    stopifnot("cluster_colored must be a logical type" = typeof(cluster_colored) == "logical")

    x_name <- colnames(cluster$ordered_data)[1]
    y_name <- colnames(cluster$ordered_data)[2]

    cl_col <- 1

    if(cluster_colored) cl_col <- rainbow(max(cluster$clusterIds))[cluster$clusterIds]


    if(type == "reachability_cluster"){
      par(mfrow=c(2,1))
    }

    if(type == "reachability_cluster" | type == "cluster"){
      plot(cluster$ordered_data, main=head(titles, 1), xlab=x_name, ylab=y_name, col = cl_col, pch = 20)
    }

    if(type == "reachability_cluster" | type == "reachability"){
      if(orderline) polygon(cluster$ordered_data[,1], cluster$ordered_data[,2])

      bp <- barplot(height = cluster$reachability, main=tail(titles, 1), xlab="data points", ylab="Reachability score",
                    col = cl_col, border = cl_col, names = seq_along(cluster$reachability))
    }
}

plot_cluster.dbscan <- function(){

}
