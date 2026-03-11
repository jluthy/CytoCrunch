test_that("plotClusterHeatmap stores a heatmap PlotClass in plotList", {
  csv_path <- system.file("extdata", "Blood3k.ExtNode.csv", package = "CytoCrunch")
  flowData <- new("CytoCrunch", sampleName = "Blood3k", inputCSVFilePath = csv_path)
  flowData <- performClustering(flowData, runID = "SLK8", method = "FlowSOM", k = 8)

  flowData <- plotClusterHeatmap(flowData, clusteringRunID = "SLK8")

  expect_equal(length(flowData@plotList), 1)
  expect_s4_class(flowData@plotList[[1]], "PlotClass")
  expect_equal(flowData@plotList[[1]]@plotType, "heatmap")
  expect_equal(flowData@plotList[[1]]@runID, "SLK8")
})

test_that("plotClusterHeatmap errors on invalid clusteringRunID", {
  csv_path <- system.file("extdata", "Blood3k.ExtNode.csv", package = "CytoCrunch")
  flowData <- new("CytoCrunch", sampleName = "Blood3k", inputCSVFilePath = csv_path)
  flowData <- performClustering(flowData, runID = "SLK8", method = "FlowSOM", k = 8)

  expect_error(plotClusterHeatmap(flowData, clusteringRunID = "NONEXISTENT"),
               "Clustering with the specified runID not found.")
})
