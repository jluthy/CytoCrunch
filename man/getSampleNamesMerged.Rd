% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CytoCrunchMerged.R
\name{getSampleNamesMerged}
\alias{getSampleNamesMerged}
\title{Get Sample Names of Merged Objects}
\usage{
getSampleNamesMerged(mergedObject, ...)
}
\arguments{
\item{mergedObject}{A CytoCrunchMerged object}
}
\value{
A character vector of sample names
}
\description{
Get the sample names from a CytoCrunchMerged object
}
\examples{
# Create a CytoCrunchMerged object
mergedObject <- new("CytoCrunchMerged",
                 sampleNames = c("Sample1", "Sample2"),
                 runIDs = c("Run1", "Run2"),
                 embeddingTypes = c("tSNE", "UMAP"),
                 mergedEmbeddings = list(matrix(NA, 5, 2), matrix(NA, 5, 2)),
                 mergedClusters = list(factor(1:5), factor(1:5)),
                 additionalData = list(list(), list())
                 )
# Get the sample names
sampleNames <- getSampleNamesMerged(mergedObject)
}
