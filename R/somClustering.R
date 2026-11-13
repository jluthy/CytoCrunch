# SOM clustering without FlowSOM/Bioconductor: a self-organising map from
# EmbedSOM (CRAN), then hierarchical clustering (Ward) of the node codebook
# vectors into k metaclusters. Each cell takes the metacluster of its node.
# Labels will not match FlowSOM's consensus metaclustering exactly.
.clusterSOM <- function(X, k, xdim = 10, ydim = 10, rlen = 10, seed = NULL) {
  .requireSuggested("EmbedSOM", "SOM clustering")
  nNodes <- xdim * ydim
  if (!is.numeric(k) || length(k) != 1L || is.na(k) || k < 1 || k > nNodes) {
    stop("'k' must be a single number between 1 and the number of SOM nodes (",
         nNodes, ").", call. = FALSE)
  }
  if (!is.null(seed)) set.seed(seed)
  som <- EmbedSOM::SOM(X, xdim = xdim, ydim = ydim, rlen = rlen, batch = TRUE)
  nodeClusters <- stats::cutree(stats::hclust(stats::dist(som$codes), method = "ward.D2"), k = k)
  node <- som$mapping[, 1]
  list(clusters = unname(nodeClusters[node]), codes = som$codes,
       nodeClusters = nodeClusters, mapping = node, xdim = xdim, ydim = ydim)
}
