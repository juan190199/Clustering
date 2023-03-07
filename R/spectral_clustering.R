calculate_mercer_kernel <- function(data, kernel_type="gauß", ...){
  # Check that Cluster Data has the right type
  source("R/utils.R")
  check_input_data(data)

  if(kernel_type == "gauß"){
    kernel_function <- function(x, y, ...) {
      optional_params <- list(...)
      if(is.null(optional_params$gamma) ){
        gamma <- 10
      }
      return(exp(- gamma * euc_dist(x, y)))
    }
  }
  else if(kernel_type == "test"){
    # Todo find other kernels
  }
  else{
    stop("Not a right kernel type provided, check in the documentary for allowed types.")
  }
  data_length <- nrow(data)

  kernel_matrix <- matrix(nrow=data_length, ncol=data_length)

  for(i in seq(to=data_length)){
    for(j in seq(from=i, to=data_length)){
      kernel_matrix[i, j] <- kernel_function(data[i, ], data[j, ], ...)
      # use symmetry of the kernel
      kernel_matrix[j, i] <- kernel_matrix[i, j]
    }
  }
  return(kernel_matrix)
}


calculate_diagonal_matrix <- function(mercer_kernel){
  # check mercer_kernel
  source("R/utils_spectral.R")
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
  source("R/utils.R")

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

calculate_k_projection <- function(data, dim_k, ...){
  mercer_kernel <- calculate_mercer_kernel(data, ...)
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
  return(t(betas))
}


spectral_clustering <- function(data, num_clusters, dim_k, ...){
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

  alphas <- calculate_k_projection(data, dim_k=dim_k, ...)

}

m <- matrix(1:12, nrow=3)
print(spectral_clustering(m, dim_k=2, num_clusters = 3))
