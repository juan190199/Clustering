library(tidyverse)

x <- y <- seq(-100, 100, length = 150)

distr <- function(x, y, mu, cov, dim){
  res <- c()
  for(i in 1:length(x)){
    for(j in 1:length(y)){
      vec <- matrix(c(x[i],y[j]), 2, 1)
      res <- append(res, as.double((1/(sqrt(((2*pi)^dim)*det(cov))))*exp((-0.5*t(vec-mu))%*%solve(cov)%*%(vec-mu))))
    }
  }
  matrix(res, length(x), length(y))
}

getx <- function(x,y, a, b, z){
  res <- matrix(ncol = 2)
  init <- TRUE
  
  for(i in 1:length(x)){
    for(j in 1:length(y)){
      if(z[i,j] >= a & z[i,j] <= b){
        if(init) {res <- rbind(round(c(x[i], y[j]), digits = 2)); init <- FALSE; next;}
        res <- rbind(res, round(c(x[i], y[j]), digits = 2))
      }
    }
  }
  res
}


z <- distr(x,y, matrix(c(0,0), 2, 1), matrix(c(14,13,8,29), 2, 2), 2)
zz <- distr(x,y, matrix(c(-10,-30), 2, 1), matrix(c(14,13,8,29), 2, 2), 2)

aa <- getx(x,y,0.000000001,0.000000004,z + zz)
aa[!is.finite(aa)] <- 0

#persp(x,y,0.45*zz + z)

plot(0,0, xlim = c(-100, 100), ylim = c(-100, 100))

nrm <- function(x, y){
  sqrt((x[1] - y[1])^2 + (x[2] - y[2])^2)
}
cdc <- function(z){
  res <- rbind(z[1,])
  sel <- res
  
  lapply(1:(nrow(z) - 1), function(i){
    elem <- res[i,]
    dis <- 100000
    indx <- 0
    
    lapply(1:nrow(z), function(j){
      
      if((nrm(elem, z[j,]) < dis)){
        dis <<- nrm(elem, z[j,])
        sel <<- z[j,]
        indx <<- j
      }
    })
    res <<- rbind(res, c(sel))
    z <<- z[-indx,]
  })
  res
}


for(i in 10^(4:20))
{
  bb <- getx(x,y,1/i,3/i,z + zz)
  polypath(cdc(bb)[,1], cdc(bb)[,2])
}



#polypath(cdc(aa)[,1], cdc(aa)[,2])











