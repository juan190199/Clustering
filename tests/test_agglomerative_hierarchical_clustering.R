# Use testthat::test_dir() to recursively search and run all test files

library(testthat)

source("R/utils.R")

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