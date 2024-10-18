#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# CytoCrunchMerged Definition
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#' @title CytoCrunchMerged
#' @description A class for storing merged CytoCrunch objects
#' @slot sampleNames A character vector of sample names.
#' @slot runIDs A character vector of run IDs.
#' @slot embeddingTypes A character vector of embedding types.
#' @slot mergedEmbeddings A list of merged embeddings.
#' @slot mergedClusters A list of merged clusters.
#' @slot additionalData A list of additional data.
#' @export
#' @examples
#' # Create a CytoCrunchMerged object
#' mergedObject <- new("CytoCrunchMerged",
#'                    sampleNames = c("Sample1", "Sample2"),
#'                    runIDs = c("Run1", "Run2"),
#'                    embeddingTypes = c("tSNE", "UMAP"),
#'                    mergedEmbeddings = list(matrix(NA, 5, 2), matrix(NA, 5, 2)),
#'                    mergedClusters = list(factor(1:5), factor(1:5)),
#'                    additionalData = list(list(), list())
#'                    )
#' # Print the CytoCrunchMerged object
#' print(mergedObject)
setClass("CytoCrunchMerged",
         slots = list(
           sampleNames = "character",
           runIDs = "character",
           embeddingTypes = "character",
           mergedEmbeddings = "list",
           mergedClusters = "list",
           additionalData = "list"
         ),
         prototype = list(
           sampleNames = character(),
           runIDs = character(),
           embeddingTypes = character(),
           mergedEmbeddings = list(),
           mergedClusters = list(),
           additionalData = list()
         )
)
# Helper Function to find the correct clustering object by runID in CytoCrunch object
getClusteringByRunID <- function(cytoCrunchObject, runID) {
  matchObj <- lapply(cytoCrunchObject@clusteringList, function(x) if (x@runID == runID) return(x))
  matchObj <- Filter(Negate(is.null), matchObj)

  if (length(matchObj) > 0) {
    return(matchObj[[1]])  # Return the first match (assuming unique runID)
  } else {
    stop(paste("No clustering object found with the given runID:", runID))
  }
}

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
#' @title mergeCytoCrunchObjects
#' @description Merge multiple CytoCrunch objects into a single CytoCrunchMerged object
#' @param objects A list of CytoCrunch objects to merge.
#' @param runIDs A character vector of run IDs corresponding to each CytoCrunch object.
#' @return A CytoCrunchMerged object containing the merged data.
#' @export
#' @examples
#' # Create two CytoCrunch objects
#' obj1 <- new("CytoCrunch",
#'            sampleName = "Sample1",
#'            inputCSVFilePath = "data/sample1.csv"
#'            )
#' obj2 <- new("CytoCrunch",
#'           sampleName = "Sample2",
#'           inputCSVFilePath = "data/sample2.csv"
#'           )
#' # Merge the CytoCrunch objects
#' mergedObject <- mergeCytoCrunchObjects(list(obj1, obj2), c("Run1", "Run2"))
#' print(mergedObject)
setGeneric("mergeCytoCrunchObjects", function(objects, runIDs) {
  standardGeneric("mergeCytoCrunchObjects")
})
setMethod("mergeCytoCrunchObjects",
          signature(objects = "list"),
          function(objects, runIDs) {

            if (length(objects) < 2) {
              stop("Please provide at least two CytoCrunch objects for merging.")
            }

            sampleNames <- sapply(objects, function(x) x@sampleName)
            embeddingTypes <- list()
            mergedEmbeddings <- list()
            mergedClusters <- list()

            # Loop through each CytoCrunch object to extract embeddings and clusters
            for (i in seq_along(objects)) {
              obj <- objects[[i]]
              runID <- runIDs[[i]]

              # Use the getEmbeddingByRunID function to get the embedding
              embeddingObj <- getEmbeddingByRunID(obj, runID)
              if (!is.null(embeddingObj)) {
                embeddings <- embeddingObj@result$reducedData
              } else {
                stop(paste("No embedding found for runID", runID, "in sample", obj@sampleName))
              }

              method <- embeddingObj@method
              embeddingTypes[[i]] <- method

              # Use the getClusteringByRunID function to get the clustering
              clusterObj <- getClusteringByRunID(obj, runID)
              if (!is.null(clusterObj)) {
                clusters <- clusterObj@result$clusters
              } else {
                stop(paste("No clustering found for runID", runID, "in sample", obj@sampleName))
              }

              # Store for merging
              mergedEmbeddings[[i]] <- embeddings
              mergedClusters[[i]] <- clusters
            }

            # Combine embeddings, clusters and ids into single matrices
            # mergedEmbeddingsMatrix <- do.call(rbind, mergedEmbeddings)
            # mergedClustersFactor <- factor(unlist(mergedClusters))
            # idsVector <- unlist(ids)
            embeddingTypesVector <- unlist(embeddingTypes)

            # Add 'id' as a new column in the merged embeddings
            # mergedEmbeddingsMatrix <- cbind(mergedEmbeddingsMatrix, id = idsVector)

            # Create a new CytoCrunchMerged object
            mergedObject <- new("CytoCrunchMerged",
                                sampleNames = sampleNames,
                                runIDs = runIDs,
                                embeddingTypes = embeddingTypesVector,
                                mergedEmbeddings = mergedEmbeddings,
                                mergedClusters = mergedClusters)

            return(mergedObject)
          }
)
#' @title Plot Merged Embeddings
#' @description Plot the merged embeddings from a CytoCrunchMerged object
#' @param mergedObject A CytoCrunchMerged object containing the merged embeddings
#' @return A grid of plots showing the merged embeddings
#' @export
#' @examples
#' # Create a CytoCrunchMerged object
#' mergedObject <- new("CytoCrunchMerged",
#'                   sampleNames = c("Sample1", "Sample2"),
#'                   runIDs = c("Run1", "Run2"),
#'                   embeddingTypes = c("tSNE", "UMAP"),
#'                   mergedEmbeddings = list(matrix(NA, 5, 2), matrix(NA, 5, 2)),
#'                   mergedClusters = list(factor(1:5), factor(1:5)),
#'                   additionalData = list(list(), list())
#'                   )
#' # Plot the merged embeddings
#' plotMergedEmbeddings(mergedObject)
setGeneric("plotMergedEmbeddings", function(mergedObject, ...) {
  standardGeneric("plotMergedEmbeddings")
})
setMethod("plotMergedEmbeddings",
          signature(mergedObject = "CytoCrunchMerged"),
          function(mergedObject) {

            # Check if there are embeddings to plot
            if (length(mergedObject@mergedEmbeddings) < 1) {
              stop("No embeddings available to plot.")
            }

            numEmbeddings <- length(mergedObject@mergedEmbeddings)
            plotList <- list()

            # Loop through each embedding and create individual plots
            for (i in seq_along(mergedObject@mergedEmbeddings)) {
              embeddings <- mergedObject@mergedEmbeddings[[i]]
              clusters <- mergedObject@mergedClusters[[i]]
              runID <- mergedObject@runIDs[i]
              methodLabel <- mergedObject@embeddingTypes[[i]]  # Get method label

              # Create a data frame for plotting
              plotData <- data.frame(
                X = embeddings[, 1],
                Y = embeddings[, 2],
                cluster = as.factor(clusters)
              )

              # Generate the plot
              p <- ggplot(plotData, aes(x = X, y = Y, color = cluster)) +
                geom_point(alpha = 0.6) +
                labs(title = paste(toupper(methodLabel), "Embedding for Run ID:", runID),
                     color = "Cluster") +
                theme_minimal()

              # Store each plot
              plotList[[i]] <- p
            }

            # Arrange the plots side by side
            gridExtra::grid.arrange(grobs = plotList, ncol = numEmbeddings)
          }
)
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Basic Object Queries
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#' @title Get Sample Names of Merged Objects
#' @description Get the sample names from a CytoCrunchMerged object
#' @param mergedObject A CytoCrunchMerged object
#' @return A character vector of sample names
#' @export
#' @examples
#' # Create a CytoCrunchMerged object
#' mergedObject <- new("CytoCrunchMerged",
#'                  sampleNames = c("Sample1", "Sample2"),
#'                  runIDs = c("Run1", "Run2"),
#'                  embeddingTypes = c("tSNE", "UMAP"),
#'                  mergedEmbeddings = list(matrix(NA, 5, 2), matrix(NA, 5, 2)),
#'                  mergedClusters = list(factor(1:5), factor(1:5)),
#'                  additionalData = list(list(), list())
#'                  )
#' # Get the sample names
#' sampleNames <- getSampleNamesMerged(mergedObject)
setGeneric("getSampleNamesMerged", function(mergedObject, ...) {
  standardGeneric("getSampleNamesMerged")
})
setMethod("getSampleNamesMerged",
          signature(mergedObject = "CytoCrunchMerged"),
          function(mergedObject) {
            return(mergedObject@sampleNames)
          }
)
#' @title Get Run IDs
#' @description Get the run IDs from a CytoCrunchMerged object
#' @param mergedObject A CytoCrunchMerged object
#' @return A character vector of run IDs
#' @export
#' @examples
#' # Create a CytoCrunchMerged object
#' mergedObject <- new("CytoCrunchMerged",
#'                 sampleNames = c("Sample1", "Sample2"),
#'                 runIDs = c("Run1", "Run2"),
#'                 embeddingTypes = c("tSNE", "UMAP"),
#'                 mergedEmbeddings = list(matrix(NA, 5, 2), matrix(NA, 5, 2)),
#'                 mergedClusters = list(factor(1:5), factor(1:5)),
#'                 additionalData = list(list(), list())
#'                 )
#' # Get the run IDs
#' runIDs <- getRunIDsMerged(mergedObject)
setGeneric("getRunIDsMerged", function(mergedObject, ...) {
  standardGeneric("getRunIDsMerged")
})
setMethod("getRunIDsMerged",
          signature(mergedObject = "CytoCrunchMerged"),
          function(mergedObject) {
            return(mergedObject@runIDs)
          }
)
