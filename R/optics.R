library(tidyverse)

# get neares k points of p
knn <- function(data, p, k){
  
  if(nrow(data) < 1) return(NULL)
  
  # calc distance to p of the k closest points  
  data %>% mutate(dist_p = sqrt((x-p$x)^2+(y-p$y)^2)) %>% 
    arrange(dist_p) %>% head(k)
}

optics <- function(data, minPts) {
  raw_data <- data
  
  colnames(data) <- c("x", "y")
  
  # init
  reachability <- c()
  data <- as_tibble(data)
  p <- data[1,]
  pnts <- rbind(as.double(p))
  scores <- knn(data, p, minPts)
  
  repeat{ 
    
    # remove current point 
    data <- setdiff(data, p)
    
    # remove current point of scores
    scores <- scores %>% filter(!(x == p$x & y == p$y))
    
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
  
  list(raw_data = raw_data ,reachability = reachability, pnts = pnts)
}


new_optics <- function(data, minPts){
  optc <- optics(data = data, minPts = minPts)
  structure(optc, class = "optics")
}

plot.optics <- function(obj){
  par(mfrow=c(2,1))
  plot(obj$raw_data, main="Order plot", xlab="X value", ylab="Y value")
  polygon(obj$pnts[,1], obj$pnts[,2])
  barplot(obj$reachability, main="Reachability plot", xlab="X value", ylab="Y value")
}

print.optics <- function(obj){
  cat("reachability list:\n")
  print(obj$reachability)
  cat("\nordered data:\n")
  print(obj$pnts)
}


# Example usage
set.seed(2)
n <- 400

data <- cbind(
  runif(4, 0, 1) + rnorm(n, sd = 0.1),
  runif(4, 0, 1) + rnorm(n, sd = 0.1)
)

start_time <- Sys.time()

#n <- 110
#data <- cbind(x = c(runif(n, min=0, max=3), runif(n, min=5, max=6), runif(n/2, min=8, max=10)),
# y = c(runif(n, min=0, max=3), runif(n, min=2, max=7), runif(n/2, min=6, max=10)))

res <- new_optics(data, minPts = 10)
plot(res)
print(res)

end_time <- Sys.time()
print(end_time - start_time)
#plot(data, col=rep(1:4, time = 100))



















