check_cluster_data <- function(data){
  stopifnot("The cluster values have to be integer or double."=
              is.double(data) || is.integer(data))
  stopifnot("The cluster data need to be provided as a matrix."=
              is.matrix(data))
}
