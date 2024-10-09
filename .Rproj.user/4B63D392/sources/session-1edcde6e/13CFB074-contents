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
           inputCSV = "matrix",
           parameterNames = "list",
           flowFrameFJ = "ANY",
           clusteringList = "list",
           dimReductionList = "list",
           batchCorrectionList = "list",
         )
)
