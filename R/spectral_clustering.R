calculate_mercer_kernel <- function(data, kernel_type="gauß", ...){
  # Check that Cluster Data has the right type
  source("R/utils_spectral.R")
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


spectral_clustering <- function(data, num_clusters, ...){
  num_clusters <- as.integer(num_clusters)
  num_datapoints <- nrow(data)
  # check input data, data gets checked in subfunctions
  stopifnot("Number of clusters has to be smaller than the number of datapoints"=
              num_clusters <= num_datapoints)
  stopifnot("Number of clusters have to be taller than one."=
              num_clusters > 1)

  mercer_kernel <- calculate_mercer_kernel(data, ...)
  diagonal_matrix <- calculate_diagonal_matrix(mercer_kernel)
  laplacian_matrix <- diagonal_matrix - mercer_kernel

  d_nsquare <- diagonal_matrix**(-1/2)
  d_nsquare[d_nsquare == Inf] <- 0
  eigenvectors <- calculate_eigenvectors(
    d_nsquare %*% laplacian_matrix %*% d_nsquare
    )

  # calculate the betas from Def. 10.50 Stefan Richter
  betas <- apply(
    eigenvectors[, seq(from=2, to=num_clusters+1)],
    2,
    function(x){
      res_vec <- num_datapoints**(-1/2)*d_nsquare*x
      return(transpose(res_vec))
    }
  )

  return(betas)

}

m <- matrix(1:4, nrow=2)
spectral_clustering(m, 1)
