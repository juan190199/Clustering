calculate_laplacian_matrix <- function(){

}

calculate_mercer_kernel(data, kernel_type="gauß"){
  # ToDo check type of data and type
  if(kernel_type == "gauß"){
    kernel_function <- function(x, y, gamma) {
      distance_eukl <- sum(outer(x,y, (x-y)**2 ) )
      exp(- gamma * distance_eukl)
    }
  else if(){
    # Todo find other kernels
  }
  else{
    stop("Not a right kernel type provided, check in the documentary for allowed types.")
  }

  return(
      outer(data, data, kernel_function)
    )
  }
}

calculate_diagonal_matrix <- function(){

}

spectral_clustering <- function(){

}
