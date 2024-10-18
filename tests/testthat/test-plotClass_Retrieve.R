test_that("plotClass stores and retrieves plot correctly", {
  csv_path1 <- system.file("extdata", "Blood3k.ExtNode.csv", package = "CytoCrunch")
  # Create the two object to be merged
  flowData <- new("CytoCrunch",
                  sampleName = "Blood3k",
                  inputCSVFilePath = csv_path1
  )
  # Cluster each object
  flowData <- performClustering(flowData, runID = "SLK8", method = "FlowSOM", k =12)
  # Embed each object
  flowData <- performEmbedding(flowData, runID = "SLK8", method = "umap", fjSeed = 23,
                               fjXdim = 10, fjYdim = 10, fjRLEN = 15, fjK = 10, fjAdjust = 0,
                               fjSmooth = 0, fjUmap.Dist = 0.5, fjMapSize = 2000)



  # Extract the embeddings and cluster labels
  embedding <- flowData@dimReductionList[[1]]@result[["reducedData"]]
  clusters <- flowData@clusteringList[[1]]@result[["clusters"]]

  # Ensure the number of rows match
  if (nrow(embedding) != length(clusters)) {
    stop("The number of rows in the embedding and clustering results do not match.")
  }

  # Create a data frame for plotting
  plotDF <- data.frame(X = embedding[, 1], Y = embedding[, 2], cluster = factor(clusters))

  # Plot using ggplot2
  library(ggplot2)
  plotObject <- ggplot(plotDF, aes(x = X, y = Y, color = cluster)) +
    geom_point(alpha = 0.7) +
    theme_minimal() +
    labs(x = "Dim 1", y = "Dim 2") +
    scale_color_discrete(name = "Cluster")

  # Create plotClass object
  plotClassObj <- createPlotClass(runID = "SLK8", plotType = "ggplot", plotObject = plotObject)

  # Check if the plot can be retrieved
  expect_silent(print(plotClassObj@plotObject))
})
