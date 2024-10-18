test_that("ClusteringClass creation works", {
  clusterObj <- new("ClusteringClass", runID = "testRun", method = "Phenograph", result = list(matrix(NA, 5, 5)), methodSpecificResults = list())
  expect_equal(clusterObj@runID, "testRun")
  expect_equal(clusterObj@method, "Phenograph")
  expect_true(is.list(clusterObj@result))
})
