#######################################################################
##         UMAP with a reusable uwot model                           ##
#######################################################################
# Format tag written into saved model files. Bump the trailing number if the
# layout of the saved list ever changes.
.MODEL_FORMAT <- "cytocrunch-uwot-1"

# Installed uwot version; a function so tests can mock the legacy code path.
.uwotVersion <- function() {
  package_version(getNamespaceVersion("uwot")[["version"]])
}

# Build the threading/seed arguments shared by umap() and umap_transform().
# uwot >= 0.2.0 supports reproducible multithreaded runs (batch SGD + seed).
# Older versions get no extra arguments and a set.seed() call instead.
.uwotParallel <- function(seed, nThreads) {
  if (!is.numeric(seed) || length(seed) != 1L || is.na(seed)) {
    stop("'seed' must be a single number.", call. = FALSE)
  }
  seed <- as.integer(seed)
  if (.uwotVersion() >= "0.2.0") {
    if (is.null(nThreads)) {
      nThreads <- max(1L, parallel::detectCores() - 1L, na.rm = TRUE)
    }
    if (!is.numeric(nThreads) || length(nThreads) != 1L || is.na(nThreads) || nThreads < 1) {
      stop("'nThreads' must be a single positive number.", call. = FALSE)
    }
    nThreads <- as.integer(nThreads)
    list(path = "threaded", threads = nThreads, seed = seed,
         args = list(n_threads = nThreads, n_sgd_threads = nThreads,
                     batch = TRUE, seed = seed))
  } else {
    set.seed(seed)
    list(path = "legacy", threads = NA_integer_, seed = seed, args = list())
  }
}

.checkRunID <- function(runID) {
  if (!is.character(runID) || length(runID) != 1L || is.na(runID) || !nzchar(runID)) {
    stop("'runID' must be a single non-empty string.", call. = FALSE)
  }
}

.getX <- function(object) {
  X <- object@inputCSV$X
  if (is.null(X) || !is.matrix(X) || nrow(X) == 0 || is.null(colnames(X))) {
    stop("Input data is empty. Please load data before running an embedding.", call. = FALSE)
  }
  X
}

# Add a DimRedClass to the object; a run with the same runID is replaced so
# lookups by runID stay unambiguous.
.storeDimRed <- function(object, entry) {
  ids <- vapply(object@dimReductionList, function(x) x@runID, character(1))
  idx <- match(entry@runID, ids)
  if (is.na(idx)) {
    object@dimReductionList[[length(object@dimReductionList) + 1L]] <- entry
  } else {
    object@dimReductionList[[idx]] <- entry
  }
  object
}

#######################################################################
##         runUwot                                                   ##
#######################################################################
#' @title Run UMAP with uwot and keep the model
#' @description Runs \code{uwot::umap(ret_model = TRUE)} directly on the
#'   expression matrix and stores the coordinates together with the fitted model
#'   in a \code{DimRedClass} entry. The model can be written to disk with
#'   \code{\link{saveEmbeddingModel}} and reused later with
#'   \code{\link{applyEmbeddingModel}} ("Apply on Map").
#'
#'   If a run with the same \code{runID} already exists it is replaced.
#'
#' @details With uwot >= 0.2.0 the run uses \code{n_threads}, \code{n_sgd_threads}
#'   and \code{batch = TRUE} with a fixed \code{seed}, so results are reproducible
#'   for a given thread count. Older uwot versions run without those arguments
#'   after \code{set.seed(seed)}; the thread count is then recorded as \code{NA}.
#'
#' @param object A CytoCrunch object
#' @param runID A unique identifier for this run
#' @param nComponents Number of output dimensions
#' @param nNeighbors Number of nearest neighbors
#' @param minDist Minimum distance between embedded points
#' @param metric Distance metric (lower case), any metric supported by uwot
#' @param seed Random seed
#' @param markers Optional character vector of column names of the expression
#'   matrix to use as features. Defaults to all columns (except \code{y}).
#' @param y Optional name of a column of the expression matrix holding cluster
#'   labels for supervised UMAP. Zeros are treated as unlabeled. The column is
#'   not used as a feature unless it is also listed in \code{markers}.
#' @param targetWeight Weight of the labels in supervised UMAP (0 to 1)
#' @param nThreads Number of threads. Defaults to
#'   \code{max(1, parallel::detectCores() - 1)}.
#' @param ... Currently unused
#' @return The CytoCrunch object with a new \code{DimRedClass} added.
#' @examples
#' \donttest{
#' blood <- new("CytoCrunch", sampleName = "Blood3k",
#'              inputCSVFilePath = system.file("extdata", "Blood3k.ExtNode.csv",
#'                                             package = "CytoCrunch"))
#' blood <- runUwot(blood, runID = "U1", nNeighbors = 15, nThreads = 1)
#' head(getEmbedding(blood, "U1"))
#' }
#' @docType methods
#' @export
setGeneric("runUwot", function(object, runID, ...) {
  standardGeneric("runUwot")
})
#' @rdname runUwot
setMethod("runUwot",
          "CytoCrunch",
          function(object, runID, nComponents = 2L, nNeighbors = 15L, minDist = 0.01,
                   metric = "euclidean", seed = 42L, markers = NULL, y = NULL,
                   targetWeight = 0.5, nThreads = NULL, ...) {
            .checkRunID(runID)
            X <- .getX(object)
            if (!is.character(metric) || length(metric) != 1L) {
              stop("'metric' must be a single string.", call. = FALSE)
            }
            metric <- tolower(metric)

            # Labels for supervised UMAP: 0 = unlabeled
            yVec <- NULL
            if (!is.null(y)) {
              if (!is.character(y) || length(y) != 1L || !(y %in% colnames(X))) {
                stop("'y' must be the name of one column of the data. Not found: '",
                     paste(y, collapse = ", "), "'. Available columns: ",
                     paste(colnames(X), collapse = ", "), call. = FALSE)
              }
              yVec <- X[, y]
              yVec[yVec == 0] <- NA
              if (all(is.na(yVec))) {
                stop("Column '", y, "' has no labels (all values are 0 or missing).", call. = FALSE)
              }
              yVec <- as.factor(yVec)
            }

            # Features
            if (is.null(markers)) {
              markers <- setdiff(colnames(X), y)
            } else {
              markers <- unique(as.character(markers))
              missing <- setdiff(markers, colnames(X))
              if (length(missing) > 0) {
                stop("Marker(s) not found in the data: ", paste(missing, collapse = ", "),
                     call. = FALSE)
              }
            }
            if (length(markers) == 0) stop("No markers left to use as features.", call. = FALSE)
            Xf <- X[, markers, drop = FALSE]
            if (anyNA(Xf)) stop("The selected markers contain missing values.", call. = FALSE)

            par <- .uwotParallel(seed, nThreads)
            args <- c(list(X = Xf, n_components = nComponents, n_neighbors = nNeighbors,
                           min_dist = minDist, metric = metric, ret_model = TRUE),
                      if (!is.null(yVec)) list(y = yVec, target_weight = targetWeight),
                      par$args)
            model <- do.call(uwot::umap, args)

            embeddingType <- if (is.null(yVec)) "UMAP" else "sUMAP"
            entry <- new("DimRedClass")
            entry@runID <- runID
            entry@method <- "uwot"
            entry@result <- list(reducedData = model$embedding)
            entry@methodSpecificResults <- list(
              model = model,
              markers = markers,
              embeddingType = embeddingType,
              nComponents = ncol(model$embedding),
              settings = list(nNeighbors = nNeighbors, minDist = minDist, metric = metric,
                              y = y, targetWeight = if (is.null(yVec)) NULL else targetWeight),
              seed = par$seed,
              threadsUsed = par$threads,
              threadPath = par$path,
              uwotVersion = as.character(.uwotVersion()),
              runID = runID,
              modelRunID = runID
            )
            .storeDimRed(object, entry)
          }
)

#######################################################################
##         Save / load a self-contained model                        ##
#######################################################################
#' @title Save a UMAP model to a single file
#' @description Writes the uwot model of a run, together with its metadata, into
#'   one \code{.rds} file. The model is embedded as raw bytes, so the file does
#'   not depend on any absolute path and keeps working after it is moved to
#'   another folder or machine.
#' @param object A CytoCrunch object
#' @param runID The runID of a run created by \code{\link{runUwot}} or
#'   \code{\link{applyEmbeddingModel}}
#' @param path Where to write the file
#' @param ... Currently unused
#' @return \code{path}, invisibly
#' @examples
#' \donttest{
#' blood <- new("CytoCrunch", sampleName = "Blood3k",
#'              inputCSVFilePath = system.file("extdata", "Blood3k.ExtNode.csv",
#'                                             package = "CytoCrunch"))
#' blood <- runUwot(blood, runID = "U1", nThreads = 1)
#' f <- tempfile(fileext = ".rds")
#' saveEmbeddingModel(blood, "U1", f)
#' }
#' @docType methods
#' @export
setGeneric("saveEmbeddingModel", function(object, runID, path, ...) {
  standardGeneric("saveEmbeddingModel")
})
#' @rdname saveEmbeddingModel
setMethod("saveEmbeddingModel",
          "CytoCrunch",
          function(object, runID, path, ...) {
            entry <- getEmbeddingByRunID(object, runID)
            info <- entry@methodSpecificResults
            if (is.null(info$model)) {
              stop("Run '", runID, "' does not hold a uwot model. ",
                   "Create one with runUwot().", call. = FALSE)
            }
            tmp <- tempfile(fileext = ".uwot")
            on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
            uwot::save_uwot(info$model, tmp)
            bytes <- readBin(tmp, what = "raw", n = file.size(tmp))
            info$model <- NULL
            saveRDS(list(format = .MODEL_FORMAT, modelBytes = bytes, metadata = info), path)
            invisible(path)
          }
)

#' @title Load a saved UMAP model
#' @description Reads a file written by \code{\link{saveEmbeddingModel}}.
#' @param path Path to the model file
#' @return A list of class \code{CytoCrunchEmbeddingModel} with elements
#'   \code{model} (the uwot model) and \code{metadata}.
#' @export
loadEmbeddingModel <- function(path) {
  notModel <- function(why) {
    stop("'", path, "' is not a CytoCrunch embedding model file (", why, ").", call. = FALSE)
  }
  if (!is.character(path) || length(path) != 1L || !file.exists(path)) {
    stop("Model file not found: ", paste(path, collapse = ", "), call. = FALSE)
  }
  obj <- tryCatch(readRDS(path), error = function(e) notModel("not a readable R data file"))
  if (!is.list(obj) || !identical(obj$format, .MODEL_FORMAT) || !is.raw(obj$modelBytes)) {
    notModel(paste0("expected format '", .MODEL_FORMAT, "'"))
  }
  tmp <- tempfile(fileext = ".uwot")
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  writeBin(obj$modelBytes, tmp)
  model <- uwot::load_uwot(tmp)
  structure(list(model = model, metadata = obj$metadata), class = "CytoCrunchEmbeddingModel")
}

#######################################################################
##         Apply on Map                                              ##
#######################################################################
#' @title Place new events into a saved UMAP map
#' @description Transforms the data in a CytoCrunch object with a previously
#'   fitted UMAP model (\code{uwot::umap_transform}). Columns are matched to the
#'   model by name and reordered to the model's training order. Extra columns are
#'   ignored; a missing marker is an error.
#' @param object A CytoCrunch object holding the new data
#' @param saved A model from \code{\link{loadEmbeddingModel}}, or a path to a
#'   model file
#' @param runID A unique identifier for this run
#' @param seed Random seed. Defaults to the seed of the saved model.
#' @param nThreads Number of threads. Defaults to
#'   \code{max(1, parallel::detectCores() - 1)}.
#' @param ... Currently unused
#' @return The CytoCrunch object with a new \code{DimRedClass} added. Its
#'   \code{modelRunID} is the runID of the model it came from.
#' @examples
#' \donttest{
#' path <- system.file("extdata", "Blood3k.ExtNode.csv", package = "CytoCrunch")
#' blood <- new("CytoCrunch", sampleName = "Blood3k", inputCSVFilePath = path)
#' blood <- runUwot(blood, runID = "U1", nThreads = 1)
#' f <- tempfile(fileext = ".rds")
#' saveEmbeddingModel(blood, "U1", f)
#' blood <- applyEmbeddingModel(blood, f, runID = "U1_applied", nThreads = 1)
#' }
#' @docType methods
#' @export
setGeneric("applyEmbeddingModel", function(object, saved, runID, ...) {
  standardGeneric("applyEmbeddingModel")
})
#' @rdname applyEmbeddingModel
setMethod("applyEmbeddingModel",
          "CytoCrunch",
          function(object, saved, runID, seed = NULL, nThreads = NULL, ...) {
            .checkRunID(runID)
            if (is.character(saved)) saved <- loadEmbeddingModel(saved)
            if (!inherits(saved, "CytoCrunchEmbeddingModel")) {
              stop("'saved' must come from loadEmbeddingModel() or be a path to a model file.",
                   call. = FALSE)
            }
            meta <- saved$metadata
            X <- .getX(object)

            missing <- setdiff(meta$markers, colnames(X))
            if (length(missing) > 0) {
              stop("Cannot apply the model: marker(s) missing from the data: ",
                   paste(missing, collapse = ", "), call. = FALSE)
            }
            Xn <- X[, meta$markers, drop = FALSE]
            if (anyNA(Xn)) stop("The model markers contain missing values.", call. = FALSE)

            if (is.null(seed)) seed <- meta$seed
            par <- .uwotParallel(seed, nThreads)
            coords <- do.call(uwot::umap_transform,
                              c(list(X = Xn, model = saved$model), par$args))

            entry <- new("DimRedClass")
            entry@runID <- runID
            entry@method <- "uwot_transform"
            entry@result <- list(reducedData = coords)
            meta$model <- saved$model
            meta$seed <- par$seed
            meta$threadsUsed <- par$threads
            meta$threadPath <- par$path
            meta$uwotVersion <- as.character(.uwotVersion())
            meta$modelRunID <- meta$runID
            meta$runID <- runID
            entry@methodSpecificResults <- meta
            .storeDimRed(object, entry)
          }
)

#######################################################################
##         Helpers for plugins                                       ##
#######################################################################
#' @title Embedding accessors
#' @description \code{getEmbedding} returns the coordinates matrix of a run.
#'   \code{getEmbeddingInfo} returns a list with \code{type} (\code{"UMAP"} or
#'   \code{"sUMAP"} for uwot runs, otherwise the method name), \code{nDims},
#'   \code{modelRunID} (the run that holds the model; \code{NA} if none) and
#'   \code{threadsUsed} (\code{NA} if not applicable).
#' @param object A CytoCrunch object
#' @param runID The runID of an embedding
#' @return A matrix (\code{getEmbedding}) or a list (\code{getEmbeddingInfo}).
#' @name embeddingAccessors
#' @examples
#' \donttest{
#' blood <- new("CytoCrunch", sampleName = "Blood3k",
#'              inputCSVFilePath = system.file("extdata", "Blood3k.ExtNode.csv",
#'                                             package = "CytoCrunch"))
#' blood <- runUwot(blood, runID = "U1", nThreads = 1)
#' getEmbeddingInfo(blood, "U1")
#' }
NULL

#' @rdname embeddingAccessors
#' @export
setGeneric("getEmbedding", function(object, runID) {
  standardGeneric("getEmbedding")
})
#' @rdname embeddingAccessors
setMethod("getEmbedding", "CytoCrunch", function(object, runID) {
  getEmbeddingByRunID(object, runID)@result$reducedData
})

#' @rdname embeddingAccessors
#' @export
setGeneric("getEmbeddingInfo", function(object, runID) {
  standardGeneric("getEmbeddingInfo")
})
#' @rdname embeddingAccessors
setMethod("getEmbeddingInfo", "CytoCrunch", function(object, runID) {
  entry <- getEmbeddingByRunID(object, runID)
  info <- entry@methodSpecificResults
  list(type = if (is.null(info$embeddingType)) entry@method else info$embeddingType,
       nDims = ncol(entry@result$reducedData),
       modelRunID = if (is.null(info$modelRunID)) NA_character_ else info$modelRunID,
       threadsUsed = if (is.null(info$threadsUsed)) NA_integer_ else info$threadsUsed)
})

#' @title Get event identifiers
#' @description Returns the SeqGeq \code{CellId} values if the export had them,
#'   otherwise the FlowJo \code{EventNumberDP} values, as a plain vector. Returns
#'   \code{NULL} if the export had neither.
#' @param object A CytoCrunch object
#' @return A vector, or \code{NULL}.
#' @export
setGeneric("getEventIds", function(object) {
  standardGeneric("getEventIds")
})
#' @rdname getEventIds
setMethod("getEventIds", "CytoCrunch", function(object) {
  cellIds <- object@inputCSV$cellIds
  if (!is.null(cellIds) && ncol(cellIds) > 0) return(as.vector(cellIds))
  eventNumbers <- object@inputCSV$eventNumbers
  if (!is.null(eventNumbers) && ncol(eventNumbers) > 0) return(as.vector(eventNumbers))
  NULL
})
