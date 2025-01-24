#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Main Class Definition
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#' @title The CytoCrunch Class
#' @description
#' The CytoCrunch object is a representation of a gated population of cells that
#' are exported from a FlowJo plugin process. These files loaded into the object
#' can be either .fcs files or .csv files containing expression data from a flow
#' cytometery experiment. The object stores metadata as well as representations
#' of low-dimensional data from clustering or dimensionality reductions methods.
#'
#' Each CytoCrunch object has a number of slots which store information. Key
#' slots to access are listed below.
#'
#' @slot sampleName The unique sample name for the selected file
#' @slot inputCSV The expression matrix exported from FlowJo
#' @slot parameterNames The metadata for parameter and stain names
#' @slot flowFrameFJ The expression matrix stored as flowFrame
#' @slot clusteringList A list of ClusteringClass objects
#' @slot dimReductionList A list of DimRedClass objects
#' @slot batchCorrectionList A list of BatchCorrectionClass objects
#' @docType class
#' @name CytoCrunch-class
setClass("CytoCrunch",
         slots = list(
           sampleName = "character",
           inputCSV = "list",
           parameterNames = "list",
           flowFrameFJ = "ANY",
           clusteringList = "list",
           dimReductionList = "list",
           batchCorrectionList = "list",
           plotList = "list"
         ),
         prototype = list(
           sampleName = "SampleName",
           inputCSV = list(X = matrix(),
                           eventNumbers = matrix()
           ),
           parameterNames = list(fjParNames = NULL,
                                 fjStainNames = NULL
           ),
           flowFrameFJ = list(ffFJ=list()
           ),
           clusteringList = list(),
           dimReductionList = list(),
           batchCorrectionList = list(),
           plotList = list()
         )
)
#' @rdname CytoCrunch-class
#' @export
#' @docType class
#' @name CytoCrunch
#' @aliases CytoCrunch
#' @format An object of class \code{CytoCrunch} is a list of slots that store information about the sample, expression data, and results of clustering and dimensionality reduction methods.
#' Initialize an empty plotList when creating a new CytoCrunch object
setMethod("initialize", "CytoCrunch",
          function(.Object, sampleName, inputCSVFilePath, ...) {
            # Call the parent class's initialize method to set up slots
            callNextMethod(.Object, ...)
            # Assign the sampleName to the new CytoCrunch object
            .Object@sampleName <- sampleName
            # Load the FlowJo CSV data
            .Object <- loadFlowJoCSV(.Object, inputCSVFilePath)
            .Object@clusteringList <- list()
            .Object@dimReductionList <- list()
            .Object@plotList <- list()

            return(.Object)

})
#######################################################################
##         Load a file from single csv file     ##
#######################################################################
#' @title Load data into CytoCrunch object
#' @return Returns the newly created object
#' @param object A CytoCrunch object
#' @param csvPath The path to the exported csv file from FlowJo
#' @param eventNumber The column name for event numbers or cell ids
#' @importFrom data.table fread
#' @importFrom data.table :=
#' @examples \dontrun{
#' inputCSVpath <- "/Documents/ExtNode.csv"
#' myobject <- loadFlowJoCSV(object, inputCSVpath, eventNumber = "EventNumberDP")}
#' @docType methods
#' @export
setGeneric("loadFlowJoCSV", function(object, csvPath, eventNumber = "EventNumberDP") {
  standardGeneric("loadFlowJoCSV")
})
#' @rdname loadFlowJoCSV
setMethod("loadFlowJoCSV",
          "CytoCrunch",
          function(object, csvPath, eventNumber = "EventNumberDP") {
            # Ensure data.table is available
            if (!requireNamespace("data.table", quietly = TRUE)) {
              stop("data.table package is not available")
            }

            # Using fread from data.table
            data <- data.table::fread(csvPath, check.names = FALSE)

            # Initialize empty matrix for eventNumbers
            eventNumbers <- matrix(nrow = nrow(data), ncol = 0)

            # Handle specified eventNumber column
            if (eventNumber %in% names(data)) {
              eventNumbers <- as.matrix(data[[eventNumber]])
              # Remove specified eventNumber column using set()
              data.table::set(data, j = eventNumber, value = NULL)
            } else {
              warning(paste("Column", eventNumber, "not found in the provided CSV. eventNumbers will be left empty."))
            }

            # Extracting parameter and stain names
            colNames <- names(data)
            parNames <- sapply(colNames, function(x) unlist(strsplit(x, " :: "))[1])
            stNames <- sapply(colNames, function(x) {
              parts <- unlist(strsplit(x, " :: "))
              if(length(parts) > 1) parts[2] else NA
            })

            # Trim whitespace from column names
            names(data) <- trimws(names(data))

            # Update object slots
            object@parameterNames <- list(fjParNames = parNames, fjStainNames = stNames)
            object@inputCSV <- list(X = as.matrix(data),
                                    eventNumbers = as.matrix(eventNumbers, dimnames = list(NULL, "EventNumber")))

            return(object)
          }
)
#######################################################################
##         Perform Clustering     ##
#######################################################################
#' @title Perform clustering on the data
#' @return Returns the CytoCrunch object with the new ClusteringClass object added
#' @param object A CytoCrunch object
#' @param runID A unique identifier for this run
#' @param method The clustering method to use
#' @param k The number of clusters to create
#' @param ... Additional arguments to pass to the method
#' @examples \dontrun{ performClustering(object, runID = "808", method = "FlowSOM", k = 10)}
#' @docType methods
#' @importFrom FlowSOM ReadInput BuildSOM BuildMST metaClustering_consensus
#' @importFrom dplyr na_if
#' @importFrom EmbedSOM SOM RandomMap uwotCoords
#' @importFrom EmbedSOM tSNECoords
#' @importFrom uwot umap
#' @export
setGeneric("performClustering", function(object, runID, method = "FlowSOM", k = NULL, ...) {
  standardGeneric("performClustering")
})
#' @rdname performClustering
setMethod("performClustering",
          "CytoCrunch",
          function(object, runID = "808", method = "FlowSOM", k = NULL, ...) {

            # Create a new ClusteringClass for this run
            clusteringObj <- new("ClusteringClass")
            clusteringObj@runID <- runID
            clusteringObj@method <- method
            clusteringObj@nClust <- k

            # Perform the clustering based on the method selected
            if (method == "FlowSOM") {
              flowSOMRes <- FlowSOM::ReadInput(object@inputCSV$X, transform = FALSE, scale = FALSE)
              flowSOMRes <- FlowSOM::BuildSOM(flowSOMRes, xdim = 10, ydim = 10)
              flowSOMRes <- FlowSOM::BuildMST(flowSOMRes)
              # Perform meta-clustering (e.g., 10 clusters) on the grid nodes
              metaClusters <- FlowSOM::metaClustering_consensus(flowSOMRes$map$codes, k = k)

              # Get the SOM grid assignment for each data point
              somAssignments <- flowSOMRes$map$mapping[, 1]

              # Map each data point's SOM grid assignment to its meta-cluster
              clusterLabels <- metaClusters[somAssignments]

              # Store the cluster labels for each point
              clusteringObj@result <- list(clusters = clusterLabels)

            } else if (method == "Phenograph") {
              # TODO: add code here for FastPG or other clustering options
              clusteringObj@result <- list(clusters = clusters$membership)
            }

            # Add clusteringObj to CytoCrunch object's list of results
            object@clusteringList[[length(object@clusteringList) + 1]] <- clusteringObj

            return(object)
          }
)
#######################################################################
##         Perform Dimensionality Reduction with EmbedSOM     ##
#######################################################################
#' @title Perform dimensionality reduction with EmbedSOM
#' @return Returns the CytoCrunch object with the new DimRedClass object added
#' @param object A CytoCrunch object
#' @param runID A unique identifier for this run
#' @param method The dimensionality reduction method to use
#' @param ... Additional arguments to pass to the method
#' @examples \dontrun{
#' performEmbedding(object, runID = "808", method = "umap", fjSeed = 123,fjXdim = 10, fjYdim = 10, fjRLEN = 15, fjK = 25, fjAdjust = 0,fjSmooth = 0, fjUmap.Dist = 0.5, fjMapSize = 5000)
#' }
#' @docType methods
#' @export
setGeneric("performEmbedding", function(object, runID, method = "umap", ...) {
  standardGeneric("performEmbedding")
})
#' @rdname performEmbedding
setMethod("performEmbedding",
          "CytoCrunch",
          function(object, runID, method ="umap", fjSeed, fjXdim, fjYdim, fjRLEN, fjK, fjAdjust, fjSmooth,
                   fjUmap.Dist, fjMapSize, supLabel, fjSuperWeight) {
            # Ensure EmbedSOM is available
            if (!requireNamespace("EmbedSOM", quietly = TRUE)) {
              stop("EmbedSOM package is not available")
            }

            # Check if input data exists
            if (is.null(object@inputCSV$X) || nrow(object@inputCSV$X) == 0) {
              stop("Input data is empty. Please load data before performing embeddings.")
            }

            # Create a new ClusteringClass for this run
            dimReductionObj <- new("DimRedClass")
            dimReductionObj@runID <- runID
            dimReductionObj@method <- method

            # Use the data matrix from the CytoCrunch object
            tmpRet <- object@inputCSV$X

            # Perform different embeddings based on flags
            if (method == "embedsom") {
              map <- EmbedSOM::SOM(object@inputCSV$X, xdim = fjXdim, ydim = fjYdim, parallel = TRUE, batch = TRUE, rlen = fjRLEN)
            }
            if (method == "tsne") {
              map <- EmbedSOM::RandomMap(object@inputCSV$X, min(fjMapSize, nrow(object@inputCSV$X)), coordsFn = EmbedSOM::tSNECoords())
            }
            if (method == "umap") {
              map <- EmbedSOM::RandomMap(object@inputCSV$X, min(fjMapSize, nrow(object@inputCSV$X)), coordsFn = EmbedSOM::uwotCoords(min_dist = fjUmap.Dist))
            }
            if (method == "supervised") {
              tempMat <- as.data.frame(object@inputCSV$X)
              clusterMat <- as.matrix(tempMat[supLabel])
              clusterMat <- dplyr::na_if(clusterMat, 0)
              clusterMat <- as.factor(clusterMat)
              map <- EmbedSOM::RandomMap(tempMat, min(fjMapSize, nrow(object@inputCSV$X)),
                                         coordsFn = EmbedSOM::uwotCoords(uwotFn = uwot::umap,
                                                                         min_dist = fjUmap.Dist,
                                                                         y = clusterMat,
                                                                         target_weight = fjSuperWeight))
            }

            dimReductionObj@result <- list(reducedData = EmbedSOM::EmbedSOM(data=object@inputCSV$X, map=map, parallel=T, k=fjK, adjust=fjAdjust, smooth=fjSmooth))

            # Add clusteringObj to CytoCrunch object's list of results
            object@dimReductionList[[length(object@dimReductionList) + 1]] <- dimReductionObj

            return(object)
          }
)
#######################################################################
##         Plotting     ##
#######################################################################
#' @title Plot an embedding with clusters
#' @description This function plots an embedding with clusters overlaid.
#' @return Returns a ggplot2 object
#' @param object A CytoCrunch object
#' @param embeddingRunID The runID of the embedding to plot
#' @param clusteringRunID The runID of the clustering to overlay
#' @examples \dontrun{ plotEmbeddingWithClusters(object, embeddingRunID = "SLK9", clusteringRunID = "SLK8")}
#' @docType methods
#' @importFrom ggplot2 ggplot geom_point theme_minimal ggtitle labs scale_color_discrete
#' @export
#' @aliases plotEmbeddingWithClusters
#' @format A ggplot2 object
#' @rdname plotEmbeddingWithClusters
setGeneric("plotEmbeddingWithClusters", function(object, embeddingRunID, clusteringRunID, ...) {
  standardGeneric("plotEmbeddingWithClusters")
})
setMethod("plotEmbeddingWithClusters",
          "CytoCrunch",
          function(object, embeddingRunID, clusteringRunID) {

            # Find the embedding corresponding to embeddingRunID
            embeddingObj <- NULL
            for (embed in object@dimReductionList) {
              if (embed@runID == embeddingRunID) {
                embeddingObj <- embed
                break
              }
            }
            if (is.null(embeddingObj)) {
              stop("Embedding with the specified runID not found.")
            }

            # Find the clustering corresponding to clusteringRunID
            clusteringObj <- NULL
            for (cluster in object@clusteringList) {
              if (cluster@runID == clusteringRunID) {
                clusteringObj <- cluster
                break
              }
            }
            if (is.null(clusteringObj)) {
              stop("Clustering with the specified runID not found.")
            }

            # Extract the embeddings and cluster labels
            embedding <- embeddingObj@result$reducedData
            clusters <- clusteringObj@result$clusters

            # Ensure the number of rows match
            if (nrow(embedding) != length(clusters)) {
              stop("The number of rows in the embedding and clustering results do not match.")
            }

            # Create a data frame for plotting
            plotDF <- data.frame(X = embedding[, 1], Y = embedding[, 2], cluster = factor(clusters))

            # Plot using ggplot2
            library(ggplot2)
            p <- ggplot(plotDF, aes(x = X, y = Y, color = cluster)) +
              geom_point(alpha = 0.7) +
              theme_minimal() +
              ggtitle(paste("Embedding (", embeddingRunID, ") with Clusters (", clusteringRunID, ")", sep = "")) +
              labs(x = "Dim 1", y = "Dim 2") +
              scale_color_discrete(name = "Cluster")

            print(p)
          }
)


#######################################################################
##         BASIC SLOT QUERIES     ##
#######################################################################
#' @title View the sample name
#' @description This function returns the sample name from the CytoCrunch object.
#' @return Returns the sampleName
#' @param object A CytoCrunch object
#' @rdname getSampleName
#' @examples \dontrun{
#' getSampleName(object)}
#' @docType methods
#' @export
setGeneric("getSampleName", function(object) {
  standardGeneric("getSampleName")
})
#' @rdname getSampleName
setMethod("getSampleName",
          "CytoCrunch",
          function(object){
            samName <- object@sampleName
            return(samName)
          }
)
#'
#' @title View parameter names
#' @description This function returns the parameter names if present from the exported population
#' @return Returns the parameter names from the exported population
#' @param object A CytoCrunch object
#' @rdname getParNames
#' @examples \dontrun{
#' getParNames(object)}
#' @docType methods
#' @export
setGeneric("getParNames", function(object) {
  standardGeneric("getParNames")
})
#' @rdname getParNames
setMethod("getParNames",
          "CytoCrunch",
          function(object){
            if(is.null(object@parameterNames$fjParNames)) {
              print("Parameter Names: None detected")
              return(NULL)
            } else {
              parNames <- object@parameterNames$fjParNames
              return(parNames)
            }
          })
#' @title View stain names
#' @description This function returns the stain names if present from the exported population
#' @return Returns the stain names if present from the exported population
#' @param object A CytoCrunch object
#' @rdname getStainNames
#' @examples \dontrun{
#' getStainNames(object)}
#' @docType methods
#' @export
setGeneric("getStainNames", function(object) {
  standardGeneric("getStainNames")
})
#' @rdname getStainNames
setMethod("getStainNames",
          "CytoCrunch",
          function(object){
            if(is.null(object@parameterNames$fjStainNames)) {
              print("Stain Names: None detected")
              return(NULL)
            } else {
              stNames <- object@parameterNames$fjStainNames
              return(stNames)
            }
})
