blood3k <- function() {
  new("CytoCrunch", sampleName = "Blood3k",
      inputCSVFilePath = system.file("extdata", "Blood3k.ExtNode.csv", package = "CytoCrunch"))
}
getClustering <- function(obj, runID) obj@clusteringList[[1]]

test_that("SOM clustering returns k clusters for every cell", {
  skip_if_not_installed("EmbedSOM")
  obj <- performClustering(blood3k(), runID = "S1", method = "SOM", k = 12, seed = 1)
  cl <- getClustering(obj, "S1")
  expect_s4_class(cl, "ClusteringClass")
  expect_equal(cl@method, "SOM")
  expect_equal(length(cl@result$clusters), 3000)
  expect_equal(sort(unique(cl@result$clusters)), 1:12)
  expect_equal(dim(cl@methodSpecificResults$codes), c(100, ncol(obj@inputCSV$X)))
})

test_that("SOM clustering is reproducible with a seed", {
  skip_if_not_installed("EmbedSOM")
  a <- performClustering(blood3k(), runID = "S1", method = "SOM", k = 8, seed = 7)
  b <- performClustering(blood3k(), runID = "S1", method = "SOM", k = 8, seed = 7)
  expect_identical(getClustering(a, "S1")@result$clusters, getClustering(b, "S1")@result$clusters)
})

test_that("SOM clustering validates k and unknown methods", {
  skip_if_not_installed("EmbedSOM")
  expect_error(performClustering(blood3k(), runID = "S1", method = "SOM"), "'k'")
  expect_error(performClustering(blood3k(), runID = "S1", method = "SOM", k = 500), "between 1 and")
  expect_error(performClustering(blood3k(), runID = "S1", method = "nope", k = 5), "Supported")
})

test_that("SOM clusters work with the existing plot functions", {
  skip_if_not_installed("EmbedSOM")
  skip_if_not_installed("pheatmap")
  skip_if_not_installed("ggplot2")
  obj <- performClustering(blood3k(), runID = "S1", method = "SOM", k = 6, seed = 1)
  obj <- plotClusterHeatmap(obj, clusteringRunID = "S1")
  obj <- plotClusterMarkerComparison(obj, clusteringRunID = "S1", clusterID = 1)
  expect_length(obj@plotList, 2)
})
