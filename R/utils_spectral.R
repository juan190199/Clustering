check_input_data <- function(data){
  stopifnot("The cluster values have to be integer or double."=
              is.numeric(data))
  stopifnot("The cluster data need to be provided as a matrix."=
              is.matrix(data))
}

check_kernel_properties <- function(kernel){
  stopifnot("The kernel has to be symmetric."=
              isSymmetric.matrix(kernel))
  stopifnot("The kernel has to be positiv semidefinite."=
              matrixcalc::is.positive.semi.definite(kernel, tol=1e-8))
  # There is no option I know of, that can check based on the matrix
  # if the kernel function is continuous
  warning("Keep in mind, kernel function should be continuous.
          No option to check that in the coding.")
}

euc_dist <- function(x, y){
  # Check input data
  stopifnot("Input data has to be a numeric vector"=
              is.numeric(x) && is.vector(x) && is.numeric(y) && is.vector(y))
  stopifnot("Vectors have to have the same length"=
              length(x) == length(y) )
  distance <- dist(rbind(x,y))
  distance <- distance[1]
  attr(distance)
  return(distance[1])
}

euc_norm <- function(x){
  euc_dist(x, rep(0, length(x)))
}
