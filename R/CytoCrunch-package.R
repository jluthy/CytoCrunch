#' @keywords internal
"_PACKAGE"

#' @import methods
#' @importFrom stats median setNames
#' @importFrom uwot umap
NULL

# Stop with a readable message when an optional (Suggests) package is missing.
.requireSuggested <- function(pkg, purpose, hint = NULL) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop("Package '", pkg, "' is required for ", purpose, " but is not installed.",
         if (!is.null(hint)) paste0(" ", hint), call. = FALSE)
  }
  invisible(TRUE)
}
