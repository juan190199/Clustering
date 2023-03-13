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
      title <- "Spectral Clustering (Hierarchichal)"
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
  plot(cluster$data, main=title(title), col = cluster$labels, pch = 19, xlab = "PC1", ylab = "PC2")
  legend("bottomright", inset = c(-0.17, 0), legend = legend, col = seq(num_clusters), pch = 19)
}

plot_cluster.kmediod <- function(){

}

plot_cluster.kmeans <- function(){

}

plot_cluster.optics <- function(){

}

plot_cluster.dbscan <- function(){

}
