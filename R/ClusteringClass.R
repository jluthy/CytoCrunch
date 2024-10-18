#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# ClusteringClass Definition
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#' @title ClusteringClass
#' @description A class for clustering-related tasks
#' @slot runID A unique identifier for each clustering instance.
#' @slot method The name of the clustering method used.
#' @slot result The clustering result in matrix format.
#' @slot methodSpecificResults A list to store method-specific outputs.
setClass("ClusteringClass",
         slots = list(
           runID = "character",
           method = "character",
           nClust = "numeric",
           result = "list",
           methodSpecificResults = "list"
         )
)
