test_that("DimRedClass object can be created", {
  csv_path1 <- system.file("extdata", "Blood3k.ExtNode.csv", package = "CytoCrunch")
  # Example data for testing
  flowData <- new("CytoCrunch",
                  sampleName = "Blood3k",
                  inputCSVFilePath = csv_path1
  )

  # Helper Function to find the correct embedding object by runID in CytoCrunch object
  getEmbeddingByRunID <- function(cytoCrunchObject, runID) {
    matchObj <- lapply(cytoCrunchObject@dimReductionList, function(x) if (x@runID == runID) return(x))
    matchObj <- Filter(Negate(is.null), matchObj)

    if (length(matchObj) > 0) {
      return(matchObj[[1]])  # Return the first match (assuming unique runID)
    } else {
      stop(paste("No embedding object found with the given runID:", runID))
    }
  }

  # Perform a new embedding with tsne to test functionality
  flowData <- performEmbedding(flowData, runID = "SLK8", method = "tsne", fjSeed = 123,
                               fjXdim = 10, fjYdim = 10, fjRLEN = 15, fjK = 10, fjAdjust = 0,
                               fjSmooth = 0, fjMapSize = 2000)

  # Create DimRedClass object
  dimRedObj <- getEmbeddingByRunID(flowData, "SLK8")

  method <- "tsne"

  # Check if the object is of the correct class
  expect_s4_class(dimRedObj, "DimRedClass")

  # Check if the slots are initialized correctly
  expect_equal(dimRedObj@method, method)
  expect_true(is.matrix(dimRedObj@result$reducedData))
})
