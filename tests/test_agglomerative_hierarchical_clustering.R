# Use testthat::test_dir() to recursively search and run all test files

library(testthat)

source("R/utils/utils.R")
source("R/agglomerative_hierarchical_clustering.R")

init_clusters <- function(data) {
  N <- nrow(data)
  clusters <- list()
  for (i in seq(N)) {
    clusters[[i]] <- list(data[i,])
  }
  return(clusters)
}

find_closest_clusters <- function(clusters, measure) {
  min_dist <- Inf
  closest_clusters <- c(NA, NA)

  clusters_ids <- 1:length(clusters)

  for (i in 1:(length(clusters_ids) - 1)) {
    for (j in (i + 1):length(clusters_ids)) {
      dist <- measure(clusters[[clusters_ids[i]]], clusters[[clusters_ids[j]]])
      if (dist <= min_dist) {
        min_dist <- dist
        closest_clusters <- c(clusters_ids[i], clusters_ids[j])
      }
    }
  }
  return(closest_clusters)
}

merge_and_form_new_clusters <- function(clusters, ci_id, cj_id) {
  new_clusters <- list()

  for (cluster_id in 1:length(clusters)) {
    if ((cluster_id == ci_id) | (cluster_id == cj_id)) {
      next
    }
    new_clusters[[length(new_clusters) + 1]] <- clusters[[cluster_id]]
  }

  new_clusters[[length(new_clusters) + 1]] <- c(clusters[[ci_id]], clusters[[cj_id]])
  return(new_clusters)
}

######################
##    Unit tests    ##
######################

test_that("init_clusters creates a list of length N", {
  data <- matrix(rnorm(20), nrow = 10)
  clusters <- init_clusters(data)
  expect_equal(length(clusters), nrow(data))
})

# Test for find_closest_clusters function
test_that("find_closest_clusters returns the indices of the closests clusters", {
  data <- matrix(rnorm(20), nrow = 10)
  clusters <- init_clusters(data)
  measure <- get_distance_measure("single")
  closest_clusters <- find_closest_clusters(clusters, measure)
  expect_equal(length(closest_clusters), 2)
  expect_true(closest_clusters[1] != closest_clusters[2])
})

# Test for merge_and_form_new_clusters function
test_that("merge_and_form_new_clusters merges two clusters and returns a list of clusters", {
  data <- matrix(rnorm(20), nrow = 10)
  clusters <- init_clusters(data)
  new_clusters <- merge_and_form_new_clusters(clusters, 1, 2)
  expect_equal(length(new_clusters), length(clusters) - 1)
  expect_equal(length(new_clusters[[length(new_clusters)]]), length(clusters[[1]]) + length(clusters[[2]]))
})

############################
##    Integration tests   ##
############################

# Define test data sets
data1 <- data.matrix(iris[, 1:4])  # 150 observations, 4 variables
data2 <- data.matrix(mtcars[, 1:11])  # 32 observations, 11 variables
data3 <- data.matrix(USArrests[, 1:4])  # 50 observations, 4 variables

# Define expected clustering results
labels1 <- as.integer(iris$Species)
labels2 <- NULL # unknown
labels3 <- c(rep(1, 17), rep(2, 17), rep(3, 16))

# Define test cases
test_that("agglomerative_hierarchical_clustering works as expected", {

  # Test case 1: data1, K=3, single linkage
  output1 <- agglomerative_hierarchical_clustering(data1, K=3, linkage_fun="single")
  # expect_identical(output1$labels, labels1, tolerance = 0.1)
  expect_length(output1$order, nrow(data1))
  expect_length(output1$clusters, 3)
  expect_equal(output1$method, "single")

  # Test case 2: data2, K=2, complete linkage
  output2 <- agglomerative_hierarchical_clustering(data2, K=2, linkage_fun="complete")
  expect_length(output2$order, nrow(data2))
  expect_length(output2$clusters, 2)
  expect_equal(output2$method, "complete")

  # Test case 3: data3, K=3, average linkage
  output3 <- agglomerative_hierarchical_clustering(data3, K=3, linkage_fun="average")
  # expect_identical(output3$labels, labels3, tolerance = 0.1)
  expect_length(output3$order, nrow(data3))
  expect_length(output3$clusters, 3)
  expect_equal(output3$method, "average")
})

############################
##    Performance tests   ##
############################

library(simstudy)

# Define the variables and distributions
def <- defData(varname = "x", dist = "normal", formula = 0, variance = 1)

# Generate datasets of different sizes
set.seed(101)
data1 <- as.matrix(genData(10, def))
data2 <- as.matrix(genData(50, def))
data3 <- as.matrix(genData(100, def))
data4 <- as.matrix(genData(500, def))
data5 <- as.matrix(genData(1000, def))

# Define linkage function and number of clusters
linkage_fun <- "complete"
K <- 3

# Time the function execution
times <- c()
times <- c(times, system.time(agglomerative_hierarchical_clustering(data1, K, linkage_fun))["elapsed"])
times <- c(times, system.time(agglomerative_hierarchical_clustering(data2, K, linkage_fun))["elapsed"])
times <- c(times, system.time(agglomerative_hierarchical_clustering(data3, K, linkage_fun))["elapsed"])
times <- c(times, system.time(agglomerative_hierarchical_clustering(data4, K, linkage_fun))["elapsed"])
times <- c(times, system.time(agglomerative_hierarchical_clustering(data5, K, linkage_fun))["elapsed"])

# Plot the results
sizes <- c(nrow(data1), nrow(data2), nrow(data3), nrow(data4), nrow(data5))
plot(sizes, times, xlab = "Dataset size", ylab = "Time (s)", pch = 19)

###########################
##    Robustness tests   ##
###########################

set.seed(123)
data <- matrix(rnorm(10000), ncol = 10)

add_noise <- function(data, level) {
  noise <- matrix(rnorm(10000, mean = 0, sd = level), ncol = 10)
  return(data + noise)
}

noisy_data <- add_noise(data, 0.1)  # add 10% noise to the data

result <- agglomerative_hierarchical_clustering(noisy_data, K = 5, linkage_fun = "complete")
original_result <- agglomerative_hierarchical_clustering(data, K = 5, linkage_fun = "complete")

# Compare the two results
table(result$labels, original_result$labels)