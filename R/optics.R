#library(tidyverse)
#library(data.table)


# !!! do not use this function, it will not be exported !!!
# get nearest k points of p
knn <- function(data, p, k){

  if(nrow(data) < 1) return(NULL)

  # data.table radix sort 60% faster than sort for tibbles
  q <- data.table::copy(data)
  q[,"dist_p" := sqrt((x-p$x)^2+(y-p$y)^2)]

  # sort by reference
  setorder(q, dist_p)
  head(q, k)
}


# !!! do not use this function, it will not be exported !!!
# perform optics algorithm
optics <- function(data, minPts, num_cluster) {
  raw_data <- data

  # names are needed
  colnames(data) <- c("x", "y")

  # init
  reachability <- c()
  data <- as.data.table(data)

  # get some element here first
  p <- data[1,]
  pnts <- rbind(as.double(p))

  # no duplicates
  data <- dplyr::setdiff(data, p)
  scores <- knn(data, p, minPts)

  # 100% no case with endless loop
  repeat{

    # remove current point
    data <- dplyr::setdiff(data, p)

    # remove current point of scores
    scores <- scores[!(x == p$x & y == p$y)]

    a <- knn(data, p, minPts)

    if(is.null(data) | is.null(a)) break

    # merge scores with the new neighbor
    # i.e update scores
    scores <- bind_rows(scores, a)

    # get the point with smallest distance
    mm <- scores[which.min(scores$dist_p),]

    reachability <- append(reachability, as.double(mm$dist_p))

    # update new center point with the smallest distance
    p <- mm[,c("x", "y")]

    pnts <- rbind(pnts, as.double(p))
  }

  ##### extract clusters from reachability #####

  # intervall size to find local max value change in it
  # here minimum cluster size but it can be changed to a value
  # in some ratio to the input data size
  k <- minPts

  # init
  clusterIds <- rep(0, length(reachability))

  # save the index and value of the highest rate of value change
  # in the local enviroment or intervall k
  tcoords <- data.table()

  # one time execution
  tt <- TRUE

  i <- 1
  while(i <= length(reachability) - k+1)
  {
    # intervall size to find the max local value rate change
    # make sure that intervall move dont intersect with previous intervall position
    x <- reachability[i:(i+k-1)]

    mx <- max(x)
    mi <- min(x)

    # highest value change in intervall k
    mdiff <- mx - mi

    # get index of max value in reachability
    q <- match(mx, x)

    # calc index of max in intervall k for the array reachability
    max_index <- i + q - 1

    # add values
    tcoords <- bind_rows(tcoords, data.table(indx = as.integer(max_index), dff = mdiff))

    # move intervall
    i <- i + k

    # we cannot always devide reachability in equal sized intervalls k
    # make sure to calc the last intervall size k for reachability properly
    # without getting out of bounce
    if((i > length(reachability) - k) & tt){
      k <- abs(length(reachability) - i) + 1
      tt <- FALSE
    }
  }

  # order tcoords by max value change dff by reference
  setorder(tcoords, -dff)

  # get the indexes of the local max arranged by max value change dff
  indx <- tcoords$indx
  ll <- length(reachability)

  # how much clusters we want to extract
  cl_num <- num_cluster

  # minimum cluster size
  cc <- minPts

  # indexes to devide reachability properly to get the
  # cluster membership values clusterIds
  nth <- sort(head(indx,cl_num - 1))

  # make sure that no cluster has less points than cc or minPts
  # Than we want to rank the values of every intervall k by its
  # maximum local value change to get an hirachy of the most relevant clusters
  # that we choose from num_clusters
  for(i in indx[-c(1:(cl_num - 1))]){
    nth <- sort(nth)

    for(j in seq_along(nth)){

      # min distance for the first outer cluster in reachability
      if(nth[j] - 1 < cc){
        nth[j] <- i
        break
      }

      # min distance for the clusters to other in reachability
      if(is.finite(nth[j+1])){
        if(abs(nth[j+1]-nth[j]) < cc){
          nth[j] <- i
          break
        }
      }

      # min distance for the last outer cluster in reachability
      if(abs(ll - nth[j]) < cc){
        nth[j] <- i
        break
      }
    }
  }

  # devide reachability properly to get the
  # cluster membership values clusterIds, for every maximum we found
  # we mark it with 1 with index in nth
  clusterIds[nth] <- 1

  # we need external counter here
  j <- 1

  # create the cluster membership ie clusterIds
  clusterIds <- sapply(clusterIds, function(x){
    if(x == 1) j <<- j + 1
    j
  })

  # keep colnames from original data
  if(!is.null(colnames(raw_data))){
    colnames(pnts) <- colnames(raw_data)
  }else{
    colnames(pnts) <- c("x", "y")
  }

  # get the size of each cluster
  cl_sizes <- as.double(table(clusterIds))

  # warn if warn message
  if(any(cl_sizes < minPts)){
    warning(str_glue("minPts was set to high to find {num_cluster} clusters with size {minPts}, may reduce minPts or num_cluster"))
  }

  # ret
  list(raw_data = raw_data ,reachability = reachability, ordered_data = pnts, clusterIds = clusterIds)
}


#' Optics algorithm
#'
#' Use this algorithm to identify a given number of clusters. Optics can
#' identify clusters with different densities unlike Dbscan.
#'
#' Clusters can be found by looking at valleys in the ordered reachability distances of each data
#' point. Optics in its original form does not select clusters for the user instead the user must
#' found cluster by look at the reachability plot and find valleys. However this implentation tries
#' to find a given number of clusters \code{num_cluster}.
#'
#' @param data A matrix, data.frame, tibble, with two numeric colums and at least two rows
#' @param minPts A number of how many points should a cluster should at least have
#' @param num_cluster A number. How many clusters should be extracted
#' @return An object of class `optics` with components:
#' \item{raw_data}{original data}
#' \item{reachability}{the reachability scores}
#' \item{ordered_data}{the orderd poins by reachability}
#' \item{clusterIds}{the cluster assignments}
#' @examples
#' n <- 100
#' data <- cbind(xval = c(runif(n*2, min=0, max=3), runif(n, min=5, max=6), runif(n/11, min=8, max=10)),
#'               yval = c(runif(n*2, min=0, max=3), runif(n, min=2, max=7), runif(n/11, min=6, max=10)))
#'
#' res <- new_optics(data = data, minPts = 7, num_cluster = 3)
#' plot(res)
#' print(res)
#' @export
new_optics <- function(data, minPts, num_cluster){

  dd <- as.matrix(data)

  ##### validators ####

  stopifnot("data must contain numeric values" = typeof(dd) == "double" | typeof(dd) == "integer" | typeof(dd) == "numeric")
  stopifnot("data is empty or does not have rows" = !is.null(nrow(dd)))
  stopifnot("data is empty or does not have columns" = !is.null(ncol(dd)))
  stopifnot("data must have two columns" = ncol(dd) == 2)
  stopifnot("data must have at least two rows" = nrow(dd) >= 2)
  stopifnot("data may contains non finite values such as Inf, NA, NULL" = all(is.finite(dd)))

  stopifnot("minPts must be greater than 0" = minPts > 0)
  stopifnot("num_cluster must be greater than 0" = num_cluster > 0)
  stopifnot("minPts must be smaller than number of data points" = minPts < nrow(data))
  stopifnot("num_cluster must be smaller than number of data points" = num_cluster < nrow(data))

  optc <- optics(data = data, minPts = minPts, num_cluster = num_cluster)
  structure(optc, class = "optics")
}


#' Plot optics class
#'
#' @param obj A optics class, call \code{new_optics()} to create an optics object
#' @param orderline A logical to determine if the data points should be connected with lines in reachability order
#' @param type A character to decide which plot should be displayed: `reachability_cluster` show both plots, `reachability` show reachability plot
#' `cluster` show cluster plot
#' @param titles A character vector with length 2. Here the titles of the plots for reachability and cluster
#' @param cluster_colored A logical. Should the data be colored by the cluster assignments
#' @examples
#' n <- 100
#' data <- cbind(xval = c(runif(n*2, min=0, max=3), runif(n, min=5, max=6), runif(n/11, min=8, max=10)),
#'               yval = c(runif(n*2, min=0, max=3), runif(n, min=2, max=7), runif(n/11, min=6, max=10)))
#'
#' res <- new_optics(data = data, minPts = 7, num_cluster = 3)
#' plot(res)
#' print(res)
#' @export
plot.optics <- function(obj, orderline = FALSE, type = "reachability_cluster", titles = c("Cluster plot", "Reachability plot"), cluster_colored = TRUE, ...){

  # type =
  # reachability_cluster, show both plots
  # reachability, show reachability plot
  # cluster, show cluster plot

  stopifnot("type must be a character" = typeof(type) == "character")
  stopifnot("type must be a character string" = length(type) == 1)

  stopifnot("Wrong type please choose: reachability_cluster or reachability or cluster" = type == "reachability_cluster" |
              type == "reachability" | type == "cluster")

  stopifnot("titles must be a character vector" = typeof(titles) == "character")
  stopifnot("orderline must be a logical type" = typeof(orderline) == "logical")
  stopifnot("cluster_colored must be a logical type" = typeof(cluster_colored) == "logical")

  x_name <- colnames(obj$ordered_data)[1]
  y_name <- colnames(obj$ordered_data)[2]

  cl_col <- 1

  if(cluster_colored) cl_col <- rainbow(max(obj$clusterIds))[obj$clusterIds]


  if(type == "reachability_cluster"){
    par(mfrow=c(2,1))
  }

  if(type == "reachability_cluster" | type == "cluster"){
    plot(obj$ordered_data, main=head(titles, 1), xlab=x_name, ylab=y_name, col = cl_col, pch = 20)
  }

  if(type == "reachability_cluster" | type == "reachability"){
    if(orderline) polygon(obj$ordered_data[,1], obj$ordered_data[,2])

    bp <- barplot(height = obj$reachability, main=tail(titles, 1), xlab="data points", ylab="Reachability score",
                  col = cl_col, border = cl_col, names = seq_along(obj$reachability))
  }
}


#' Print optics class
#'
#' @param obj A optics class, call \code{new_optics()} to create an optics object
#' @examples
#' n <- 100
#' data <- cbind(xval = c(runif(n*2, min=0, max=3), runif(n, min=5, max=6), runif(n/11, min=8, max=10)),
#'               yval = c(runif(n*2, min=0, max=3), runif(n, min=2, max=7), runif(n/11, min=6, max=10)))
#'
#' res <- new_optics(data = data, minPts = 7, num_cluster = 3)
#' print(res)
#' @export
print.optics <- function(obj, ...){
  cat("reachability list:\n")
  print(obj$reachability)

  cat("\nordered data:\n")
  print(obj$ordered_data)

  cat("\nclusterIds for ordered data:\n")
  print(obj$clusterIds)
}




################################### Example usage ###################################################

#set.seed(2)
#n <- 506

#data <- cbind(
#  ewfeyxyx = runif(4, 0, 1) + rnorm(n, sd = 0.1),
#  yyywyxyx = runif(4, 0, 1) + rnorm(n, sd = 0.1)
#)

#start_time <- Sys.time()

#n <- 100
#data <- cbind(xval = c(runif(n*2, min=0, max=3), runif(n, min=5, max=6), runif(n/11, min=8, max=10)),
#              yval = c(runif(n*2, min=0, max=3), runif(n, min=2, max=7), runif(n/11, min=6, max=10)))

#res <- new_optics(data = data, minPts = 7, num_cluster = 3)
#plot(res)
#print(res)

#end_time <- Sys.time()
#print(end_time - start_time)
