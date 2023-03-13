#' Sanity Check Matrix Data
#'
#' `check_matrix_data` controls that `data` contains a matrix.
#'
#' @param data hopefully a numeric matrix.
check_matrix_data <- function(data){
  stopifnot("The cluster values have to be integer or double."=
              is.numeric(data))
  stopifnot("The cluster data need to be provided as a matrix."=
              is.matrix(data))
}

#' Sanity Check Kernel
#'
#' `check_kernel_properties` controls that the given matrix is a mercer kernel.
#'
#' @param kernel numeric matrix, hopefully a mercer kernel.
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

#' Sanity Check Spectral Cluster
#'
#' `check_spectral_cluster` controls that the given list is a spectral cluster.
#'
#' @param cluster list, hopefully a spectral cluster.
check_spectral_cluster <- function(cluster){
  stopifnot("Cluster has to be spectral."=
              attr(cluster, "class") == "spectral" )
  stopifnot("The kernel is missing"=
              !is.null(cluster$kernel))
  stopifnot("The projection dimension dim_k is missing"=
              !is.null(cluster$dim_k))
}


get_cluster_class <- function(func){
  if(func == "agglomerative_hierarchical_clustering") "hierarchical"
  else if(func == "kmedoid") "kmedoid"
  else if(func == "kMeans") "kmeans"
  else if(func == "optics") "optics"
  else if(func == "dbscan") "dbscan"
}
