test_that("CytoCrunchMerged object can be created", {
  csv_path1 <- system.file("extdata", "Blood3k.ExtNode.csv", package = "CytoCrunch")
  csv_path2 <- system.file("extdata", "LymphNode3k.ExtNode.csv", package = "CytoCrunch")
  # Create the two object to be merged
  flowData <- new("CytoCrunch",
                  sampleName = "Blood3k",
                  inputCSVFilePath = csv_path1
  )
  flowData2 <- new("CytoCrunch",
                   sampleName = "LymphNode3k",
                   inputCSVFilePath = csv_path2
  )
  # Cluster each object
  flowData <- performClustering(flowData, runID = "SLK8", method = "FlowSOM", k =12)

  flowData2 <- performClustering(flowData2, runID = "SLK9", method = "FlowSOM", k =15)
  # Embed each object
  flowData <- performEmbedding(flowData, runID = "SLK8", method = "tsne", fjSeed = 123,
                               fjXdim = 10, fjYdim = 10, fjRLEN = 15, fjK = 10, fjAdjust = 0,
                               fjSmooth = 0, fjMapSize = 2000)
  flowData2 <- performEmbedding(flowData2, runID = "SLK9", method = "umap", fjSeed = 23,
                                fjXdim = 10, fjYdim = 10, fjRLEN = 15, fjK = 10, fjAdjust = 0,
                                fjSmooth = 0, fjUmap.Dist = 0.5, fjMapSize = 2000)
  # Merge the objects
  mergedObj <- mergeCytoCrunchObjects(list(flowData, flowData2), c("SLK8", "SLK9"))
  # # Example data for testing
  sampleNames <- getSampleNamesMerged(mergedObj)
  runIDs <- getRunIDsMerged(mergedObj)
  methodLabels <- c("tsne", "umap")

  # Check if the object is of the correct class
  expect_s4_class(mergedObj, "CytoCrunchMerged")

  # Check if the slots are initialized correctly
  expect_equal(mergedObj@sampleNames, sampleNames)
  expect_equal(mergedObj@runIDs, runIDs)
  expect_equal(mergedObj@embeddingTypes, methodLabels)
  expect_true(is.list(mergedObj@mergedEmbeddings))
  expect_true(is.list(mergedObj@mergedClusters))
})
