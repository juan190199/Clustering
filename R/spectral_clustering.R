#'Spectral Clustering Algorithm.
#'
#'`spectral_clustering` is a clustering algorithm that reduces the dimension of
#'the training data and then performs a clustering algorithm on the new
#'transformed data.
#'
#'The first step is dimension reduction, which is accomplished with
#'`calculate_k_projection`. This packet function has to solve an eigenvalue
#'problem involving, for example, the Laplace matrix. The return provides us
#'with new data points in the dimension `dim_k` which are fed into the given
#'clustering algorithm `cluster_fun`.
#'
#'If we want to find two clusters, the optimization problem from which our
#'eigenvalue problem stems is the problem of finding a minimum cut in the
#'induced reduced ε-neighborhood graph. Where epsilon is chosen as infinity.
#'
#'
#'
#'@section Terms: All data points in the input data have to stem from an
#'  identical distribution and have to be independent.
#'  Clearly the data points also have to be numeric and from the same vector
#'  space.
#'
#'@param data data.frame, with numeric data. Each row represents one
#'  data point. The columns are the dimensions.
#'@param num_clusters numeric value larger than one, the number of clusters.
#'@param dim_k numeric value smaller than the number of data points, the
#'  dimension the data should be projected into.
#'@param cluster_fun function, a cluster function that runs on the vectors
#'  retrieved from the projection into R^`dim_k`.
#'@param arg_cluster_fun Optional arguments for the `cluster_fun` function.
#'  Possible values need to be looked up in the respective documentation.
#'@param arg_kernel Optional arguments for the `calculate_mercer_kernel`
#'  function. Types can be checked in the corresponding documentation.
#'@return The return type of `cluster_fun`
#'
#'@examples
#'cluster_data <- data.matrix(scale(iris[, 1:4]))
#'
#'spectral_clustering(
#'   cluster_data,
#'   num_clusters=2,
#'   dim_k=1,
#'   cluster_fun=agglomerative_hierarchical_clustering,
#'   arg_cluster_fun=c("single")
#')
#'@export
#'
#'@references \url{https://link.springer.com/book/10.1007/978-3-662-59354-7}
spectral_clustering <- function(
    data,
    num_clusters=2,
    dim_k=1,
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

  alphas <- calculate_k_projection(data, dim_k=dim_k, arg_kernel=arg_kernel)

  if(missing(arg_cluster_fun)){
    cluster <- cluster_fun(alphas, num_clusters)
  } else{
    cluster <- rlang::exec(cluster_fun, alphas, num_clusters, !!!arg_cluster_fun)
  }

  cluster$data <- data
  cluster$alphas <- alphas

  # get name of the cluster fun
  cluster_fun_name <- deparse(substitute(cluster_fun))
  attr(cluster, "class") <- c("spectral", get_cluster_class(cluster_fun_name))

  if(attr(cluster, "class")[2] == "kMeans" && !missing(arg_cluster_fun)){
    if(!is.null(arg_cluster_fun$type)){
      attr(cluster, "class")[2] == arg_cluster_fun$type
    }
  }

  return(cluster)
}

#' K-Projection.
#'
#'`calculate_k_projection` projects the data into the vector space R^`dim_k`
#'
#'@references \url{https://link.springer.com/book/10.1007/978-3-662-59354-7}
#'
#'
#'@section Terms:
#' All data points in the input data have to stem from an identical distribution and have to be independent.
#'
#' Clearly the data points also have to be numeric and from the same vector space.
#'@param data data.frame, with numeric data. Each row represents one
#'  data point. The columns are the dimensions.
#'@param dim_k numeric value smaller than the number of data points, the
#'  dimension the data should be projected into.
#'@param arg_kernel Optional arguments for the `calculate_mercer_kernel` function.
#'
#'TODO
#'
#' @return matrix, with the transformed data points
#' @export
calculate_k_projection <- function(data, dim_k, arg_kernel){
  stopifnot("data has to be a matrix or data.frame"=
              is.data.frame(data) || is.matrix(data))
  data <- as.matrix(data)

  mercer_kernel <- calculate_mercer_kernel(data, kernel=arg_kernel)
  diagonal_matrix <- calculate_diagonal_matrix(mercer_kernel, is_continuous = TRUE)
  laplacian_matrix <- diagonal_matrix - mercer_kernel

  d_nsquare <- diagonal_matrix**(-1/2)
  d_nsquare[d_nsquare == Inf] <- 0
  eigenvectors <- calculate_eigenvectors_symmetric(
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

#'Mercer Kernel
#'
#'`calculate_mercer_kernel` calculates for every two data points in `data` the
#'result of the kernel function specified in `arg_kernel`and stores it in a
#'matrix.
#'
#'A kernel function is a symmetric, positive definite function R^n kreuz R^n to
#'R^`dim_k`. Check the implemented kernel functions in args. If you need an
#'additional function don't hesitate to reach out and we'll try our best to
#'implement it:)
#'
#'@references \url{https://link.springer.com/book/10.1007/978-3-662-59354-7}
#'
#'@param data matrix, with numeric data. Each row represents one data point. The
#'  columns are the dimensions.
#'@param dim_k numeric value smaller than the number of data points, the
#'  dimension the data should be projected into.
#'@param kernel list, optional argument that specify the kernel. Possible
#'  options are:
#'- Gauß Kernel: Default kernel. Call with kernel=list(type="gauß", metric=?, gamma=?)
#'  Distance metrics can be chosen, check in `dist_func` for possible values.
#'  Gamma taller than zero can also be freely selected.
#'- Normed Gauß Kernel:
#'  Call with kernel=list(type="normed_gauß", metric=?, gamma=?) Same
#'  things listed in Gauß Kernel apply her as well.
#'
#'@return matrix A, where A_ij (i, j in {1, ..., length(data)}) is the result of
#'  the kernel function of ith and jth data point.
#'
#' @export
calculate_mercer_kernel <- function(data, kernel=list(type="gauß", gamma=1/10, metric="euclidean")){
  # Check that Cluster Data has the right type
  check_matrix_data(data)
  data_length <- nrow(data)

  stopifnot("Kernel has to be a list"= is.list(kernel))

  # Gauß Kernel
  if(kernel$type == "gauß"){
    if(is.null(kernel$gamma) ){
      gamma <- 10
    }
    else{
      gamma <- kernel$gamma
      stopifnot("Gamma has to be numeric"=
                  length(gamma)==1 && is.numeric(gamma))
    }
    kernel_function <- function(x, y) {
      return(calculate_gauss_kernel(x, y, gamma=gamma, metric=kernel$metric))
    }
  }
  # Normed Gauß Kernel
  else if(kernel$type == "normed_gauß"){
    if(is.null(kernel$gamma) ){
      gamma <- 10
    }
    else{
      gamma <- kernel$gamma
      stopifnot("Gamma has to be numeric"=
                  length(gamma)==1 && is.numeric(gamma))
    }
    kernel_function <- function(x, y) {
        numerator <- calculate_gauss_kernel(x, y, gamma=gamma, metric=kernel$metric)
        denominator_one <- 1/data_length * sum(apply(data, 1, calculate_gauss_kernel, x, gamma=gamma, metric=kernel$metric))
        denominator_two <- 1/data_length * sum(apply(data, 1, calculate_gauss_kernel, y, gamma=gamma, metric=kernel$metric))
        return(numerator/((denominator_one)**(1/2) * (denominator_two)**(1/2)))
    }
  }
  else{
    stop("Not a right kernel type provided, check in the documentary for allowed types.")
  }

  # Calculate the kernel matrix
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

#' Gauß Kernel
#'
#' The Gauß kernel is a kernel that is often used for spectral clustering
#'
#' @param x y numeric vectors, the input vectors that we want to calculate the
#'   kernel of.
#' @param gamma numeric positive value.
#' @param metric character, specifies the metric used in the kernel. Possible values
#'   can be checked in `dist_func`.
#'
#' @return numeric value, the result of the gauß kernel, i.e. exp(- gamma *
#'   dist_func(x, y, type=metric))
#'
#'  !!! This function won't be exported !!!
calculate_gauss_kernel <- function(x, y, gamma, metric){
  stopifnot("Gamma has to be numeric"= length(gamma)==1 && is.numeric(gamma))
  return(exp(- gamma * dist_func(x, y, type=metric)))
}

#' Diagonal Matrix of a Mercer Kernel.
#'
#' `calculate_diagonal_matrix` is a function used in the spectral clustering process.
#'
#' @param mercer_kernel numeric vectors, the input vectors that we want to calculate the
#'   kernel of.
#' @return diagonal matrix D with D_ii = sum(mercer_kernel[i,]) for i in {1,.., length(mercer_kernel)}
#'
#'!!! This function won't be exported !!!
calculate_diagonal_matrix <- function(mercer_kernel, is_continuous=FALSE){
  # check mercer_kernel
  check_matrix_data(mercer_kernel)
  # check kernel properties
  check_kernel_properties(mercer_kernel, is_continuous=is_continuous)

  n <- nrow(mercer_kernel)
  diagonal_matrix <- matrix(0, nrow = n, ncol = n)

  for(i in seq(to=n) ){
    diagonal_matrix[i, i] <- sum(mercer_kernel[i, ])
  }
  return(diagonal_matrix)
}

#' Normalized Eigen Vectors
#'
#' `calculate_eigenvectors_symmetric` calculates the eigen vectors of the given symmetric `matrix`. Then it normalizes them based on the `metric`
#'
#' @param matrix numeric symmetric matrix.
#' @param matric the metric used for the normalization
#' @return numeric matrix, contains eigen vectors from `matrix` normalized based on `metric`.
#' Eigen vectors in descending order of their eigen values.
#'
#' !!! This function won't be exported !!!
calculate_eigenvectors_symmetric <- function(matrix, metric="euclidean"){
  check_matrix_data(matrix)
  stopifnot("Matrix has to be symmetric"=isSymmetric(matrix))

  eigen_vectors <- eigen(matrix, symmetric = TRUE)$vectors
  # put eigenvectors in ascending order
  eigen_vectors <- eigen_vectors[, ncol(eigen_vectors):1]

  # normalize
  for(i in seq(to=ncol(eigen_vectors)) ) {
    norm <- norm_vec(eigen_vectors[, i])
    if( norm != 1){
      eigen_vectors[, i] <- 1/norm * eigen_vectors[, i]
    }
  }
  return(eigen_vectors)
}

# iris_std <- data.matrix(scale(iris[, 1:4]))
# cluster <- spectral_clustering(iris_std,
#                               num_clusters=3,
#                               dim_k=2,
#                               cluster_fun=new_kMeans,
#                               arg_cluster_fun = list(type="kMeans"))
# sloop::s3_dispatch(plot_cluster(cluster))
# plot_cluster(cluster)
# cluster <- spectral_clustering(iris_std,
#                               num_clusters=3,
#                               dim_k=2,
#                               cluster_fun=kmedoid)
# a <- rbind(c(2,1,1), c(1,1,2), c(1,2,1))
# cluster <- spectral_clustering(a,
#                               num_clusters=3,
#                               dim_k=2,
#                               cluster_fun=kmedoid)
