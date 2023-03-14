#' Sanity Check Matrix Data
#'
#' `check_matrix_data` controls that `data` contains a matrix.
#'
#' @param data hopefully a numeric matrix.
#'
#' !!! This function won't be exported !!!
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
#' @param is_continuous logical, the kernel has to be continuous.
#'
#'   !!! This function won't be exported !!!
check_kernel_properties <- function(kernel, is_continuous=FALSE){
  stopifnot("A kernel has to be quadratic"=
              nrow(kernel) == ncol(kernel))
  stopifnot("The kernel has to be symmetric."=
              isSymmetric.matrix(kernel))
  stopifnot("The kernel has to be positiv semidefinite."=
              matrixcalc::is.positive.semi.definite(kernel, tol=1e-8))
  # There is no option I know of, that can check based on the matrix
  # if the kernel function is continuous
  if(!is_continuous){
    warning("Keep in mind, kernel function should be continuous.
            No option to check that in the coding.")
  }
}

#' Sanity Check Spectral Cluster
#'
#' `check_spectral_cluster` controls that the given list is a spectral cluster.
#'
#' @param cluster list, hopefully a spectral cluster.
#'
#'   !!! This function won't be exported !!!
check_spectral_cluster <- function(cluster){
  stopifnot("Cluster has to be spectral."=
              attr(cluster, "class") == "spectral" )
  stopifnot("We need a second Clustering method"=
              length(attr(cluster, "class")))
  stopifnot("Clustering method isn't implemented yet."=
              attr(cluster, "class")[2] %in% c("hierarchical", "kmedoid", "kMeans", "optics", "dbscan") )
}

#' Clustering Method Spectral
#'
#' `get_cluster_class` returns the clustering method used in the spectral
#' clustering. Is used to get the class attribute for a spectral clustering
#' object.
#'
#' @param func character, the function of the clustering method as string.
#' @return character, the clustering method as string.
#'
#'   !!! This function won't be exported !!!
get_cluster_class <- function(func){
  if(func == "agglomerative_hierarchical_clustering") "hierarchical"
  else if(func == "kmedoid") "kmedoid"
  else if(func == "new_kMeans") "kMeans"
  else if(func == "optics") "optics"
  else if(func == "dbscan") "dbscan"
  else stop("No proper cluster function was provided")
}
