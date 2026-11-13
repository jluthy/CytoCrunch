blood3k <- function() {
  new("CytoCrunch", sampleName = "Blood3k",
      inputCSVFilePath = system.file("extdata", "Blood3k.ExtNode.csv", package = "CytoCrunch"))
}
# Small, fast settings
fit <- function(obj, runID = "U1", ...) {
  runUwot(obj, runID = runID, nNeighbors = 10, nThreads = 2, ...)
}
skip_if_old_uwot <- function() {
  skip_if(utils::packageVersion("uwot") < "0.2.0", "needs uwot >= 0.2.0")
}

test_that("runUwot stores coordinates, model and metadata", {
  obj <- fit(blood3k())
  emb <- getEmbedding(obj, "U1")
  expect_equal(dim(emb), c(3000, 2))
  expect_true(all(is.finite(emb)))
  info <- getEmbeddingInfo(obj, "U1")
  expect_equal(info$type, "UMAP")
  expect_equal(info$nDims, 2)
  expect_equal(info$modelRunID, "U1")
  ms <- obj@dimReductionList[[1]]@methodSpecificResults
  expect_equal(ms$markers, colnames(obj@inputCSV$X))
  expect_equal(ms$seed, 42L)
  expect_false(is.null(ms$model))
  # re-using a runID replaces the run
  obj <- fit(obj)
  expect_length(obj@dimReductionList, 1)
})

test_that("same seed and thread count gives identical coordinates", {
  skip_if_old_uwot()
  obj <- blood3k()
  a <- getEmbedding(fit(obj, "A"), "A")
  b <- getEmbedding(fit(obj, "B"), "B")
  expect_identical(a, b)
})

test_that("legacy uwot path runs without threading arguments", {
  testthat::local_mocked_bindings(.uwotVersion = function() package_version("0.1.16"))
  obj <- fit(blood3k())
  info <- getEmbeddingInfo(obj, "U1")
  expect_true(is.na(info$threadsUsed))
  expect_equal(obj@dimReductionList[[1]]@methodSpecificResults$threadPath, "legacy")
})

test_that("threaded path records the thread count", {
  skip_if_old_uwot()
  obj <- fit(blood3k())
  expect_equal(getEmbeddingInfo(obj, "U1")$threadsUsed, 2L)
  expect_equal(obj@dimReductionList[[1]]@methodSpecificResults$threadPath, "threaded")
})

test_that("supervised UMAP treats 0 as unlabeled and excludes y from features", {
  obj <- blood3k()
  lab <- rep(0, nrow(obj@inputCSV$X))
  lab[1:1500] <- rep(1:3, length.out = 1500)
  obj@inputCSV$X <- cbind(obj@inputCSV$X, cluster = lab)
  obj <- fit(obj, y = "cluster")
  expect_equal(getEmbeddingInfo(obj, "U1")$type, "sUMAP")
  ms <- obj@dimReductionList[[1]]@methodSpecificResults
  expect_false("cluster" %in% ms$markers)
  expect_error(fit(obj, "U2", y = "nope"), "nope")
})

test_that("markers subset is honoured and validated", {
  obj <- blood3k()
  m <- colnames(obj@inputCSV$X)[1:5]
  obj <- fit(obj, markers = m)
  expect_equal(obj@dimReductionList[[1]]@methodSpecificResults$markers, m)
  expect_error(fit(obj, "U2", markers = c(m, "bogus")), "bogus")
})

test_that("model save/load survives moving the file", {
  obj <- fit(blood3k())
  d1 <- tempfile("a"); d2 <- tempfile("b"); dir.create(d1); dir.create(d2)
  f1 <- file.path(d1, "model.rds")
  saveEmbeddingModel(obj, "U1", f1)
  f2 <- file.path(d2, "moved.rds")
  file.copy(f1, f2)
  unlink(d1, recursive = TRUE)

  saved <- loadEmbeddingModel(f2)
  expect_equal(saved$metadata$markers, colnames(obj@inputCSV$X))
  obj2 <- applyEmbeddingModel(obj, saved, runID = "A1", nThreads = 2)
  coords <- getEmbedding(obj2, "A1")
  expect_equal(dim(coords), c(3000, 2))
  expect_true(all(is.finite(coords)))
  info <- getEmbeddingInfo(obj2, "A1")
  expect_equal(info$modelRunID, "U1")
  expect_equal(info$type, "UMAP")
})

test_that("loadEmbeddingModel rejects other files readably", {
  f <- tempfile(fileext = ".rds")
  saveRDS(list(a = 1), f)
  expect_error(loadEmbeddingModel(f), "not a CytoCrunch embedding model")
  writeLines("junk", f)
  expect_error(loadEmbeddingModel(f), "not a CytoCrunch embedding model")
  expect_error(loadEmbeddingModel(tempfile()), "not found")
})

test_that("saveEmbeddingModel needs a uwot run", {
  skip_if_not_installed("EmbedSOM")
  obj <- performEmbedding(blood3k(), runID = "T", method = "tsne", fjSeed = 1, fjMapSize = 500,
                          fjK = 10, fjAdjust = 0, fjSmooth = 0)
  expect_error(saveEmbeddingModel(obj, "T", tempfile()), "does not hold a uwot model")
})

test_that("apply on map: reproducible, order-independent, validates markers", {
  skip_if_old_uwot()
  obj <- fit(blood3k())
  f <- tempfile(fileext = ".rds")
  saveEmbeddingModel(obj, "U1", f)
  saved <- loadEmbeddingModel(f)

  a1 <- getEmbedding(applyEmbeddingModel(obj, saved, "A", nThreads = 2), "A")
  a2 <- getEmbedding(applyEmbeddingModel(obj, saved, "A", nThreads = 2), "A")
  expect_identical(a1, a2)

  shuffled <- obj
  X <- shuffled@inputCSV$X
  shuffled@inputCSV$X <- X[, rev(colnames(X))]
  s <- getEmbedding(applyEmbeddingModel(shuffled, saved, "S", nThreads = 2), "S")
  expect_identical(a1, s)

  extra <- obj
  extra@inputCSV$X <- cbind(extra@inputCSV$X, extraCol = 1)
  e <- getEmbedding(applyEmbeddingModel(extra, saved, "E", nThreads = 2), "E")
  expect_identical(a1, e)

  missingM <- obj
  missingM@inputCSV$X <- X[, -c(1, 2)]
  expect_error(applyEmbeddingModel(missingM, saved, "M"),
               "missing from the data: .*CCR7.*CD16")
})

test_that("getEmbedding errors on an unknown runID", {
  expect_error(getEmbedding(blood3k(), "nope"), "No embedding object found")
})
