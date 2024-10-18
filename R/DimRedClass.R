#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Dimensionality Reduction Class Definition
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#' @title DimRedClass
#' @description A class for dimensionality reduction-related tasks
#' @slot runID A unique identifier for each dim reduction instance.
#' @slot method The name of the dim reduction method used.
#' @slot result The dim reduction result in matrix format.
#' @slot methodSpecificResults A list of method-specific results.
setClass("DimRedClass",
         slots = list(
           runID = "character",
           method = "character",
           result = "list",
           methodSpecificResults = "list"
         )
)
