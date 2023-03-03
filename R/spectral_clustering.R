calculate_laplacian_matrix <- function(){

}

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
      return(exp(- gamma * distance(x,y)))
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



spectral_clustering <- function(){

}
