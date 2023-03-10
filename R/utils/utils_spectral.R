check_input_data <- function(data){
  stopifnot("The cluster values have to be integer or double."=
              is.numeric(data))
  stopifnot("The cluster data need to be provided as a matrix."=
              is.matrix(data))
}

check_kernel_properties <- function(kernel){
  stopifnot("A kernel has to be quadratic"=
              nrow(kernel) == ncol(kernel))
  stopifnot("The kernel has to be symmetric."=
              isSymmetric.matrix(kernel))
  stopifnot("The kernel has to be positiv semidefinite."=
              matrixcalc::is.positive.semi.definite(kernel, tol=1e-8))
  # There is no option I know of, that can check based on the matrix
  # if the kernel function is continuous
  warning("Keep in mind, kernel function should be continuous.
          No option to check that in the coding.")
}

check_spectral_cluster <- function(cluster){
  stopifnot("Cluster has to be spectral."=
              attr(cluster, "cluster") == "spectral" )
  stopifnot("The kernel is missing"=
              !is.null(cluster$kernel))
  stopifnot("The projection dimension dim_k is missing"=
              !is.null(cluster$dim_k))
}
