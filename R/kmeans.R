library(tidyverse)

kMeans <- function(data, k = 1, maxIterations = 100, setInitialCentroids = NULL) {
  
  # choose start values for the clusers manually
  init <- ifelse(is.null(setInitialCentroids), TRUE, FALSE)
  
  # keep start value of k
  init_k <- k
  k <- as.integer(k)
  
  clusterIds <- NULL
  centroids <- setInitialCentroids
  
  if(init){
    # init the start values of the cluster membership randomly 
    clusterIds <- sample.int(k, nrow(data), replace = TRUE)
  }
  
  # loop over max iteration
  for (i in 1:maxIterations) {
    
    # Achtung es kann sein, dass centroids zusammenfallen und sich damit deren Anzahl
    # verringert im vergleich zu gesetzten k, also setzte k auf die neue Anzahl damit
    # index out of bounce verhindert wird und gebe eine Warnung aus
    num_cl <- length(unique(clusterIds))
    if(num_cl < k && init){
      k <- num_cl
    }
    
    if(init){
      
      # if no column names
      if(is.null(colnames(data))) colnames(data) <- paste("val", sep="", 1:ncol(data))
      
      # calculate the center points of the clusters
      centroids <- as_tibble(data)
      centroids <- centroids %>% add_column(cid = clusterIds)
      centroids <- centroids %>% group_by(cid) %>% summarise(across(everything(), mean))
      centroids <- as.matrix(centroids %>% select(-cid))
    }
    
    # calculate the distance to the cluster centers
    distances <- sapply(1:k, function(j) rowSums((data - centroids[j,])^2))
    
    # assign the data points to its nearest cluster
    newClusterIds <- apply(distances, 1, which.min)
    
    # check if cluster membership has changed
    if (all(clusterIds == newClusterIds) & !is.null(clusterIds)) break
    
    # The new values for the cluster membership
    clusterIds <- newClusterIds
    init <- TRUE
  }
  
  if(init_k > k){
    warning("The maximum number of centroids ist less than k ==> k was set to high or not an integer")
  }
  
  # return some cluster info
  list(data = data, clusterIds = clusterIds, clusteredData = cbind(data, clusterIds), centroids = centroids)
}

kMeans_pp <- function(data, k, maxIterations = 100) {
  
  X <- data
  
  # choose the first cluter center randomly
  centroids <- rbind(X[sample(nrow(X), 1), ])
  
  # repeat until k cluster centers were found
  while (nrow(centroids) < k) {
    
    # calculate the distance of every point to its nearest cluster center
    distances <- apply(X, 1, function(x) min(sqrt(rowSums((centroids - x)^2))))
    
    # choose the next cluster center proportional to the distance
    next_centroid <- rbind(X[sample.int(nrow(X), size = 1, prob = distances), ])
    
    # add the next cluster center to the list of cluster centers
    centroids <- rbind(centroids, next_centroid)
  }
  
  # execute kMeans with our pre-calculated cluster centers (centroids)
  kMeans(X, k, maxIterations = maxIterations, setInitialCentroids = centroids)
}



new_kMeans_cluster <- function(data, k = 1, maxIterations = 100, setInitialCentroids = NULL, type = "kMeans"){
  
  if(type == "kMeans++" && !is.null(setInitialCentroids)){
    stop("For kMeans++ algorithm it is not possible to set setInitialCentroids to a non NULL value")
  }
  
  stopifnot("Wrong type of algorithm ==> set it to kMeans or kMeans++" = type == "kMeans" | type == "kMeans++")
  stopifnot("data must be a matrix" = class(data)[1] == "matrix")
  stopifnot("data must be a matirx with numeric values" = typeof(data) == "integer" | typeof(data) == "double" | typeof(data) == "numeric")
  stopifnot("k must be greater than 0" = k > 0)
  stopifnot("k must be a number" = typeof(k) == "integer" | typeof(k) == "double")
  stopifnot("maxIterations must be greater than 0" = maxIterations > 0)
  stopifnot("maxIterations must be a number" = typeof(maxIterations) == "integer" | typeof(maxIterations) == "double")
  stopifnot("setInitialCentroids must be a matrix or NULL" = is.null(setInitialCentroids) |  class(setInitialCentroids)[1] == "matrix")
  stopifnot("setInitialCentroids must be a matirx with numeric values" = typeof(setInitialCentroids) == "integer" |  
              typeof(setInitialCentroids) == "double" | typeof(setInitialCentroids) == "numeric" | is.null(setInitialCentroids))
  stopifnot("number of rows from setInitialCentroids must be equal to k" = k == nrow(setInitialCentroids) | is.null(setInitialCentroids))
  
  
  if(type == "kMeans"){
    km <- kMeans(data = data, k = k, maxIterations = maxIterations, setInitialCentroids = setInitialCentroids)
    return(structure(km, class = "kMeans"))
  }
  
  else if(type == "kMeans++"){
    km_pp = kMeans_pp(data = data, k = k, maxIterations = maxIterations)
    return(structure(km_pp, class = "kMeans"))
  }
}

plot.kMeans <- function(obj){
  plot(obj$data, col = obj$clusterIds)
  points(obj$centroids, col = "blue", lwd = 3)
}

print.kMeans <- function(obj){
  cat("center points:\n")
  print(obj$centroids)
  cat("\ncluster membership data:\n")
  print(obj$clusteredData)
  
}












# Generiere zufällige Datenpunkte
#set.seed(123)
#data <- matrix(rnorm(1000), ncol = 2)

n <- 100
data <- cbind(c(runif(n, min=0, max=3), runif(n, min=5, max=6), runif(n, min=8, max=10)),
              c(runif(n, min=0, max=3), runif(n, min=2, max=7), runif(n, min=6, max=10)))

km <- new_kMeans_cluster(data, 3, 10, type = "kMeans++")


# Visualisiere die Ergebnisse
plot(km)
print(km)



