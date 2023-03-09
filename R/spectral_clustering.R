#'Implementation of the spectral clustering algorithm.
#'
#'`spectral_clustering` is a clustering algorithm that emphasizes the distances of the input data.
#'
#' It defines a graph that has the data points as vertices and edges with edge weights that take the distances between all data points into account.
#' Formulates an optimization problem that is in the case of two clusters the problem of finding a minimum cut in that graph.
#' This problem is equivalent to an eigenvector problem, that we solve in the function.
#'
#'@references \url{https://link.springer.com/book/10.1007/978-3-662-59354-7}
#'
#'
#'@section terms:
#' All data points in the input data have to stem from an identical distribution and have to be independent.
#'
#' Clearly the data points also have to be numeric and from the same vector space.
#'
#'@param data matrix, with columns of numeric data.
#'Each row represents one data point. The columns are the dimensions.
#'@param num_clusters numeric value larger than one, the number of clusters.
#'@param dim_k numeric value smaller than the number of data points, the dimension the data should be projected into.
#'@param cluster_fun function, a cluster function that runs on the vectors retrieved from the projection into R^`dim_k`.
#'@param arg_cluster_fun Optional arguments for the `cluster_fun` function.
#'Possible values need to be looked up in the respective documentation.
#'@param arg_kernel Optional arguments for the `calculate_mercer_kernel` function.
#'Types can be checked in the corresponding documentation.
#'@return The return type of `cluster_fun`
spectral_clustering <- function(
    data,
    num_clusters,
    dim_k,
    cluster_fun,
    arg_cluster_fun,
    arg_kernel=list(type="gauß", gamma=10, metric="euclidean")
    )
  {
  num_clusters <- as.integer(num_clusters)
  dim_k <- as.integer(dim_k)
  num_datapoints <- nrow(data)

  # check input data, data gets checked in subfunctions
  stopifnot("Number of clusters has to be smaller than the number of datapoints"=
              num_clusters <= num_datapoints)
  stopifnot("Number of clusters have to be taller than one."=
              num_clusters > 1)
  stopifnot("The dimension of the projection image has to be at least one."=
              num_clusters > 0)
  stopifnot("The dimension of the projection image has to be smaller than the number of clusters"=
              dim_k < num_clusters)

  alphas <- calculate_k_projection(data, dim_k=dim_k, arg_kernel=arg_kernel, metric=metric)

  cluster <- cluster_fun(data, num_clusters, arg_cluster_fun)

  cluster["kernel"] <- arg_kernel
  cluster["dim_k"] <- dim_k
  cluster["data"] <- data
  attr(cluster, "cluster") <- "spectral"

  return(cluster)
}

#' K-Projection.
#'
#'`calculate_k_projection` projects the data into the vector space R^`dim_k`
#'
#'@references \url{https://link.springer.com/book/10.1007/978-3-662-59354-7}
#'
#'
#'@section terms:
#' All data points in the input data have to stem from an identical distribution and have to be independent.
#'
#' Clearly the data points also have to be numeric and from the same vector space.
#'
#'@param data matrix, with columns of numeric data.
#'Each row represents one data point. The columns are the dimensions.
#'@param dim_k numeric value smaller than the number of data points, the dimension the data should be projected into.
#'@param arg_kernel Optional arguments for the `calculate_mercer_kernel` function.
#'
#'TODO
#'
#'@return matrix, with the transformed data points
calculate_k_projection <- function(data, dim_k, arg_kernel){
  mercer_kernel <- calculate_mercer_kernel(data, kernel=arg_kernel)
  diagonal_matrix <- calculate_diagonal_matrix(mercer_kernel)
  laplacian_matrix <- diagonal_matrix - mercer_kernel

  d_nsquare <- diagonal_matrix**(-1/2)
  d_nsquare[d_nsquare == Inf] <- 0
  eigenvectors <- calculate_eigenvectors(
    d_nsquare %*% laplacian_matrix %*% d_nsquare
  )

  num_datapoints <- nrow(data)
  # calculate the betas from Def. 10.50 Stefan Richter
  betas <- apply(
    eigenvectors[, seq(from=2, to=dim_k+1), drop=FALSE],
    2,
    function(x){
      res_vec <- num_datapoints**(-1/2)*d_nsquare %*% x
      return(res_vec)
    }
  )
  return(betas)
}


calculate_mercer_kernel <- function(data, kernel=list(type="gauß", gamma=10, metric="euclidean")){
  # Check that Cluster Data has the right type
  source("R/utils/utils.R")
  source("R/utils/utils_spectral.R")
  check_input_data(data)
  data_length <- nrow(data)

  stopifnot("Kernel has to be a list"= is.list(kernel))

  if(kernel$type == "gauß"){
    if(is.null(kernel$gamma) ){
      gamma <- 10
    }
    else{
      gamma <- kernel$gamma
      stopifnot("Gamma has to be numeric"=
                  length(gamma)==1 && is.numeric(gamma))
    }
    if(kernel$normed == TRUE){
      kernel_function <- function(x, y) {
        numerator <- calculate_gauss_kernel(x, y, gamma=gamma, metric=metric)
        denominator_one <- 1/data_length * sum(apply(data, 1, calculate_gauss_kernel, x, gamma=gamma, metric=metric))
        denominator_two <- 1/data_length * sum(apply(data, 1, calculate_gauss_kernel, y, gamma=gamma, metric=metric))
        return(numerator/((denominator_one)**(1/2) * (denominator_two)**(1/2)))
      }
    } else{
      kernel_function <- function(x, y) {
        return(calculate_gauss_kernel(x, y, gamma=gamma, metric=metric))
      }
    }
  }
  else if(kernel$type == "test"){
    # TODO find other kernels
  }
  else{
    stop("Not a right kernel type provided, check in the documentary for allowed types.")
  }
  kernel_matrix <- matrix(nrow=data_length, ncol=data_length)

  for(i in seq(to=data_length)){
    for(j in seq(from=i, to=data_length)){
      kernel_matrix[i, j] <- kernel_function(data[i, ], data[j, ])
      # use symmetry of the kernel
      kernel_matrix[j, i] <- kernel_matrix[i, j]
    }
  }
  return(kernel_matrix)
}

calculate_gauss_kernel <- function(x, y, gamma, metric){
  stopifnot("Gamma has to be numeric"= length(gamma)==1 && is.numeric(gamma))
  return(exp(- gamma * dist_func(x, y, type=metric)))
}


calculate_diagonal_matrix <- function(mercer_kernel){
  # check mercer_kernel
  source("R/utils/utils_spectral.R")
  check_input_data(mercer_kernel)
  # check kernel properties
  check_kernel_properties(mercer_kernel)

  n <- nrow(mercer_kernel)
  diagonal_matrix <- matrix(0, nrow = n, ncol = n)

  for(i in seq(to=n) ){
    diagonal_matrix[i, i] <- sum(mercer_kernel[i, ])
  }
  return(diagonal_matrix)
}

calculate_eigenvectors <- function(matrix){
  # import utils
  source("R/utils/utils.R")

  eigen_vectors <- eigen(matrix, symmetric=TRUE)$vectors
  # put eigenvectors in ascending order
  eigen_vectors <- eigen_vectors[, ncol(eigen_vectors):1]

  # normalize
  for(i in seq(to=ncol(eigen_vectors)) ) {
    norm <- euc_norm(eigen_vectors[, i])
    if( norm != 1){
      eigen_vectors[, i] <- 1/norm * eigen_vectors[, i]
    }
  }
  return(eigen_vectors)
}

add_point_to_spectral_cluster <- function(cluster, x){
  source("R/utils/spectral_util.R")
  check_spectral_cluster(cluster)

  stopifnot("x and the cluster data have to be in the same vector space."=
              length(x) == ncol(cluster$data))
  stopifnot("x has to be a numeric vector"=
              is.vector(x) && is.numeric(x))

  n <- nrow(data)
  dim_k <- cluster$dim_k

  normed_kernel <- calculate_mercer_kernel(data=data, kernel=c(list(normed=TRUE), cluster$kernel))

  data <- rbind(data, x)
  kernel <- calculate_mercer_kernel(data=data, kernel = cluster$kernel)

  # Calculation of the projection matrix based on definition 10.55 Richter
  projection <- vector("numeric", length = dim_k)
  prefactor <- (1/n * sum(kernel[n+1, ]))**(-1)
  # Calculate the eigen values in the formula
  eigen_values <- eigen(1/n * normed_kernel, symmetric=TRUE)$values

  for(i in seq(dim_k)){
    eigen_value <- eigen_values[i]
    sum <- sum(apply(data, )])
  }
# }
