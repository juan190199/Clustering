#' Hi
plot_cluster <- function(cluster, titel, colors, ...){
  UseMethod("plot_cluster", cluster)
}

plot_cluster.spectral <- function(cluster, titel, colors, ...){
  NextMethod()
}

plot_cluster.hierarchical <- function(cluster, titel, colors, ...){
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
