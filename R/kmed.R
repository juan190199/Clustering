#-------------------------------------------------------------------------------


dista <- function(x,y) {
    # function from stats for getting the distance matrix
    # posibility of different methods like minkowski
    dis <- dist(rbind(x,y))
    return(dis)
}
dista3D <- function(x,y) {
    # 3d distance
    dis <- sqrt((x[,1] - y[,1])^2 + (x[,2] - y[,2])^2 + (x[,3] - y[,3])^2)
    return(dis)
}


#'Implementation of a k-medoid clustering algorithm
#'
#'`kmedoid()` utilizes a PAM algorithm (partitioning around medoids) to cluster
#'data. k-medoids aim to minimize the total sum of distances within each cluster
#'to its central medoid.
#'
#'@param data A list with two or three columns of numeric data.
#'@param k The number of medoids or clusters to be computed.
#'@param iter The number of iterations the optimization should go through. If an
#'  optimal state has been reached, the function will exit.
#'@return A list containing the following elements:
#'- clusters: a vector of integers indicating the cluster assignment of each
#'  data point.
#'- medoids: a vector of integers indicating the index of the medoid for each
#'  cluster.
#'- cost: a numeric value indicating the total sum of distances within each
#'  cluster to its central medoid.
#'
#' Note: This implementation of k-medoids does not work well with small data sets.

#'@examples
#' kmedoid(iris[3:4],3,5)
#'@export
kmedoid <- function(data, k=1, iter=1){
    stopifnot('Number of medoids k has to be greater than 0!'= k > 0)
    stopifnot('Number of iterations has to be greater than 0!'= iter > 0)
    stopifnot('Clustering is only possible for 2 and 3 dimensional data so far!'= ncol(data) %in% c(2,3))
    if(k >= nrow(data)){
        cost <- 0
        medoids <- 1:nrow(data)
        clusters <- 1:nrow(data)
        warning('Number of medoids greater or equal to the number of data points!')
        return(list(clusters = clusters, medoids = medoids, cost = cost))
    }
    # create a matrix with the distance between every point of the data set
    # prime a matrix of the correct size
    dist_mat <- matrix(0,nrow(data),nrow(data))
    # distance calculation (although across() would be more elegant, it's not more
    # efficient)
    if (ncol(data) ==2){
        for (i in 1:nrow(data)){
            for (j in 1:i) {
                dist_mat[i,j] <- dista(data[i,],data[j,])
                dist_mat[j,i] <- dist_mat[i,j]
            }
        }
    } else {
        for (i in 1:nrow(data)){
            for (j in 1:i) {
                dist_mat[i,j] <- dista3D(data[i,],data[j,])
                dist_mat[j,i] <- dist_mat[i,j]
            }
        }
    }


    # select k points at random to be used as initial medoids
    medoids <- sample(1:nrow(data), k)



    # initiate the clusters, evenly distributed between each medoid for
    # as many values as possible
    clusters <- rep(1:k, each=nrow(data)%/%k)
    # if the number of points is not a multiple of k, the remaining points will be
    # assigned at random
    if (nrow(data)%% k !=0){
        clusters <- c(clusters, sample(1:k, nrow(data)%%k,replace=TRUE))
    }
    # calculate the initial cost, which is the sum of all distances within each
    # cluster
    cost <- 0
    for (i in 1:k) {
        distances <- dist_mat[which(clusters == i), medoids[i]]
        cost <- cost +sum(distances)

    }

    # iterate over clusters to optimize
    for (i in 1:iter) {
        # Assign each point to closest medoid, maybe before loop
        for (j in 1:nrow(data)) {
            if (!j %in% medoids) {
                distances <- dist_mat[j, medoids]
                closest_medoid <- which.min(distances)
                clusters[j] <- closest_medoid
            }
        }

        # Update medoids
        for (j in 1:k) {
            cluster_j <- which(clusters == j)
            if (length(cluster_j) == 0) {
                medoids[j] <- sample(1:nrow(data), 1) # set medoid to a random point
            } else {
                medoid_distances <- apply(dist_mat[cluster_j, cluster_j, drop=FALSE], 2, sum)
                medoids[j] <- cluster_j[which.min(medoid_distances)]
            }
        }


        # Calculate new cost
        new_cost <- 0
        for (j in 1:k) {
            distances <- dist_mat[which(clusters == j), medoids[j]]
            new_cost <- new_cost + sum(distances)

        }

        # Check for stable state
        if (new_cost == cost) {
            print('Reached stable configuration. Terminating early.')# todo remove maybe?
            break
        } else {
            cost <- new_cost
        }
    }

    # Return results
    return(list(clusters = clusters, medoids = medoids, cost = cost,data =data))
}
