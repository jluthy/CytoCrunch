#######################################################################
##         Cluster vs Rest Marker Comparison Plots                   ##
#######################################################################
#' @title Plot Cluster vs Rest Marker Comparison
#' @description Generates a panel of boxplots or violin plots comparing a
#'   selected cluster's marker expression against all other cells ("rest").
#'   Plots are sorted by the magnitude of difference (median of selected cluster
#'   minus median of rest), so markers most enriched in the focal cluster appear
#'   first and markers with little difference appear last.
#'
#'   The result is stored as a PlotClass object in the CytoCrunch object's
#'   plotList for later retrieval.
#'
#' @return Returns the CytoCrunch object with the comparison plot appended to
#'   plotList
#' @param object A CytoCrunch object
#' @param clusteringRunID The runID of the clustering to use
#' @param clusterID The numeric ID of the focal cluster to compare against the
#'   rest (e.g., 1, 5, 12)
#' @param plotType Plot geometry: \code{"boxplot"} (default) or \code{"violin"}
#' @param markers Optional character vector of marker/stain names to include.
#'   Defaults to all markers.
#' @param ncol Number of columns in the facet grid layout (default 4)
#' @param ... Additional arguments (currently unused, reserved for future use)
#' @examples
#' \donttest{
#' if (requireNamespace("EmbedSOM", quietly = TRUE) &&
#'     requireNamespace("ggplot2", quietly = TRUE)) {
#'   blood <- new("CytoCrunch", sampleName = "Blood3k",
#'               inputCSVFilePath = system.file("extdata", "Blood3k.ExtNode.csv",
#'                                              package = "CytoCrunch"))
#'   blood <- performClustering(blood, runID = "C1", method = "SOM", k = 10, seed = 1)
#'
#'   # Boxplots for cluster 3 vs everything else
#'   blood <- plotClusterMarkerComparison(blood, clusteringRunID = "C1",
#'                                        clusterID = 3)
#'
#'   # Violin plots for the first three markers
#'   blood <- plotClusterMarkerComparison(blood, clusteringRunID = "C1",
#'                                        clusterID = 3, plotType = "violin",
#'                                        markers = head(getStainNames(blood), 3))
#' }
#' }
#' @docType methods
#' @export
setGeneric("plotClusterMarkerComparison",
           function(object, clusteringRunID, clusterID,
                    plotType = "boxplot", markers = NULL, ncol = 4, ...) {
             standardGeneric("plotClusterMarkerComparison")
           })

#' @rdname plotClusterMarkerComparison
setMethod("plotClusterMarkerComparison",
          "CytoCrunch",
          function(object, clusteringRunID, clusterID,
                   plotType = "boxplot", markers = NULL, ncol = 4, ...) {

            .requireSuggested("ggplot2", "plotting")

            # -- Validate plotType --
            plotType <- match.arg(plotType, choices = c("boxplot", "violin"))

            # -- Find clustering by runID --
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

            clusters <- clusteringObj@result$clusters

            # -- Validate clusterID --
            availableClusters <- sort(unique(clusters))
            if (!(clusterID %in% availableClusters)) {
              stop(paste0(
                "clusterID ", clusterID, " not found in clustering results.\n",
                "Available cluster IDs: ",
                paste(availableClusters, collapse = ", ")
              ))
            }

            # -- Build expression matrix with marker labels --
            exprMat    <- object@inputCSV$X
            stainNames <- object@parameterNames$fjStainNames
            parNames   <- object@parameterNames$fjParNames
            markerLabels <- ifelse(is.na(stainNames), parNames, stainNames)
            colnames(exprMat) <- markerLabels

            # -- Optionally subset to requested markers --
            if (!is.null(markers)) {
              missing <- setdiff(markers, colnames(exprMat))
              if (length(missing) > 0) {
                available <- sort(colnames(exprMat)[!is.na(colnames(exprMat))])
                stop(paste0(
                  "Marker(s) not found: ", paste(missing, collapse = ", "), "\n",
                  "Available markers: ", paste(available, collapse = ", ")
                ))
              }
              exprMat <- exprMat[, markers, drop = FALSE]
            }

            # -- Create group label: selected cluster vs rest --
            group <- ifelse(clusters == clusterID,
                            paste0("Cluster ", clusterID),
                            "Rest")
            group <- factor(group, levels = c(paste0("Cluster ", clusterID), "Rest"))

            # -- Compute median difference per marker for sort order --
            focalIdx <- clusters == clusterID
            medianDiff <- apply(exprMat, 2, function(col) {
              median(col[focalIdx]) - median(col[!focalIdx])
            })

            # Sort by absolute difference (largest first)
            markerOrder <- names(sort(abs(medianDiff), decreasing = TRUE))

            # -- Build long-format data frame for ggplot --
            markerCols <- colnames(exprMat)
            longDF <- data.frame(
              group      = rep(group, times = length(markerCols)),
              marker     = rep(markerCols, each = nrow(exprMat)),
              expression = as.vector(exprMat),
              stringsAsFactors = FALSE
            )

            # Apply sort order via factor levels
            longDF$marker <- factor(longDF$marker, levels = markerOrder)

            # -- Build ggplot --
            group <- marker <- expression <- NULL  # column names used by aes()/facet_wrap()
            p <- ggplot2::ggplot(longDF,
                                 ggplot2::aes(x = group,
                                              y = expression,
                                              fill = group))

            if (plotType == "boxplot") {
              p <- p +
                ggplot2::geom_boxplot(outlier.size = 0.4, outlier.alpha = 0.3,
                                      linewidth = 0.4)
            } else {
              p <- p +
                ggplot2::geom_violin(trim = TRUE, linewidth = 0.4) +
                ggplot2::geom_boxplot(width = 0.15, fill = "white",
                                      outlier.size = 0.3, outlier.alpha = 0.3,
                                      linewidth = 0.3)
            }

            p <- p +
              ggplot2::facet_wrap(~ marker, scales = "free_y", ncol = ncol) +
              ggplot2::scale_fill_manual(
                values = c(stats::setNames("#00d4ff", paste0("Cluster ", clusterID)),
                           c("Rest" = "#3a5a70"))
              ) +
              ggplot2::labs(
                title    = paste0("Cluster ", clusterID,
                                  " vs Rest - Marker Expression Comparison"),
                subtitle = paste0("Clustering: ", clusteringRunID,
                                  "  |  Sorted by absolute median difference"),
                x        = NULL,
                y        = "Expression",
                fill     = "Group"
              ) +
              ggplot2::theme(
                strip.text = ggplot2::element_text(size = 9, face = "bold"),
                axis.text.x = ggplot2::element_text(angle = 30, hjust = 1, size = 8)
              )

            print(p)

            # -- Store in PlotClass and append to plotList --
            plotObj <- createPlotClass(
              runID      = clusteringRunID,
              plotType   = paste0("clusterComparison_", plotType, "_cluster", clusterID),
              plotObject = p
            )
            object@plotList[[length(object@plotList) + 1]] <- plotObj

            return(object)
          }
)
