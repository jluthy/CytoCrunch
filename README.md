
# CytoCrunch

<!-- badges: start -->
<!-- badges: end -->

The goal of CytoCrunch is to manage data exported from a FlowJo v10 workspace plugin process. The current implementation expects to load in the .csv file exported from FlowJo v10 that ends with '..ExtNode.csv' when executing a R-based plugin process. FlowJo v10 has an open source code base for plugins, so if you have an idea for a new plugin, you can create your own plugin process in Java, R, Python and more!

## Installation

You can install the development version of CytoCrunch from [GitHub](https://github.com/jluthy/CytoCrunch) with:

``` r
install.packages("devtools")
devtools::install_github("jluthy/CytoCrunch")
```

## Example

CytoCrunch is designed to work with FlowJo v10 workspace data as it is exported from FlowJo during a plugin process. You can create a new CytoCrunch object and load a csv file of expression data exported from a FlowJo plugin process like this:

``` r
library(CytoCrunch)
## basic example code

# Create a CytoCrunch object and load in csv data
flowData <- new("CytoCrunch",
                sampleName = "Blood3k",
                inputCSVFilePath = "./inst/extdata/Blood3k.ExtNode.csv")

# Perform clustering with FlowSOM
flowData <- performClustering(flowData, runID="SLK8", method="FlowSOM", k=12)

# Perform dimensionality reduction with EmbedSOM
flowData <- performEmbedding(flowData, runID = "SLK8", method = "umap", fjSeed = 23,
                              fjXdim = 10, fjYdim = 10, fjRLEN = 15, fjK = 10, fjAdjust = 0,
                              fjSmooth = 0, fjUmap.Dist = 0.5, fjMapSize = 2000)

# Generate a plot of embedding and clustering results
plotObj <- plotEmbeddingWithClusters(flowData, embeddingRunID = "SLK8", clusteringRunID = "SLK8")

```

## Contributing
Improvements and new features will be added on a regular basis, please post on
the [github page](https://github.com/jluthy/CytoCrunch) with any questions or if
you would like to contribute.