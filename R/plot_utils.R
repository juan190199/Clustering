#' Hi
plot_cluster <- function(cluster){
  UseMethod("plot_cluster")
}

plot_cluster.spectral <- function(cluster){
  NextMethod()
}

plot_cluster.hierarchical <- function(cluster){
  plot(cluster$data, col = cluster$labels, pch = 19, xlab = "PC1", ylab = "PC2")
  legend("bottomright", legend = unique(result$labels), col = 1:length(unique(result$labels)), pch = 19)
}

plot_cluster.kmediod <- function(){

}

plot_cluster.kmeans <- function(){

}

plot_cluster.optics <- function(){

}

plot_cluster.dbscan <- function(){

}
