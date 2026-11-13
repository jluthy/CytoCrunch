# CytoCrunch 0.3.0

## New
* `runUwot()` runs `uwot::umap()` directly (unsupervised, or supervised on a label
  column) and keeps the fitted model. Uses multithreaded, reproducible settings
  with uwot >= 0.2.0.
* `saveEmbeddingModel()` / `loadEmbeddingModel()` write and read a single
  self-contained model file that survives being moved between folders or machines.
* `applyEmbeddingModel()` places new data into a saved map ("Apply on Map"),
  matching columns by name and stopping with the list of any missing markers.
* `getEmbedding()`, `getEmbeddingInfo()` and `getEventIds()` helpers.
* `performClustering(method = "SOM")`: SOM clustering from EmbedSOM (CRAN) with
  Ward clustering of the SOM nodes, as an alternative to FlowSOM that needs no
  Bioconductor package. Labels differ from FlowSOM's.

## Changes
* `loadFlowJoCSV()` now removes both `EventNumberDP` (FlowJo) and `CellId`
  (SeqGeq) from the expression matrix and stores them as
  `inputCSV$eventNumbers` and `inputCSV$cellIds`. It warns only when neither is
  present, and errors if any remaining column is non-numeric.
* `FlowSOM`, `EmbedSOM`, `ggplot2`, `gridExtra` and `pheatmap` moved from
  Imports to Suggests, so loading and UMAP need only `data.table` and `uwot`.
  Functions that need them stop with a clear message if they are missing.
* `dplyr` is no longer used.

## Fixes
* `performClustering(method = "Phenograph")` now stops with "not implemented yet"
  instead of failing on an undefined variable; unknown methods also error.
* `loadDemoData()` and `plotClusterMarkerComparison()` are now exported.

# CytoCrunch 0.2.0

* Initial version.
