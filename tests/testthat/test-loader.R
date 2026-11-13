write_csv <- function(...) {
  path <- tempfile(fileext = ".csv")
  df <- data.frame(..., check.names = FALSE)
  utils::write.csv(df, path, row.names = FALSE)
  path
}
load_csv <- function(path) new("CytoCrunch", sampleName = "t", inputCSVFilePath = path)

test_that("both EventNumberDP and CellId are removed from X and stored", {
  p <- write_csv(`Comp-FITC-A :: CCR7` = c(1.5, 2, 3), `Comp-APC-A :: CD95` = c(4, 5, 6),
                 EventNumberDP = 1:3, CellId = c(10L, 20L, 30L))
  expect_silent(obj <- load_csv(p))
  expect_equal(colnames(obj@inputCSV$X), c("Comp-FITC-A :: CCR7", "Comp-APC-A :: CD95"))
  expect_equal(colnames(obj@inputCSV$eventNumbers), "EventNumberDP")
  expect_equal(colnames(obj@inputCSV$cellIds), "CellId")
  expect_equal(as.vector(obj@inputCSV$cellIds), c(10, 20, 30))
  expect_equal(getEventIds(obj), c(10, 20, 30))
})

test_that("a CSV with only one ID column works without a warning", {
  p1 <- write_csv(A = c(1, 2), EventNumberDP = 1:2)
  expect_silent(o1 <- load_csv(p1))
  expect_equal(colnames(o1@inputCSV$X), "A")
  expect_equal(ncol(o1@inputCSV$cellIds), 0)
  expect_equal(getEventIds(o1), c(1, 2))

  p2 <- write_csv(A = c(1, 2), CellId = 5:6)
  expect_silent(o2 <- load_csv(p2))
  expect_equal(colnames(o2@inputCSV$X), "A")
  expect_equal(ncol(o2@inputCSV$eventNumbers), 0)
})

test_that("neither ID column gives exactly one warning", {
  p <- write_csv(A = c(1, 2), B = c(3, 4))
  expect_warning(o <- load_csv(p), "Neither")
  expect_null(getEventIds(o))
})

test_that("non-numeric columns are an error that names them", {
  p <- write_csv(A = c(1, 2), label = c("x", "y"), EventNumberDP = 1:2)
  expect_error(load_csv(p), "label")
})

test_that("parameter/stain split and trimmed names are unchanged", {
  obj <- new("CytoCrunch", sampleName = "Blood3k",
             inputCSVFilePath = system.file("extdata", "Blood3k.ExtNode.csv", package = "CytoCrunch"))
  expect_equal(unname(obj@parameterNames$fjParNames[1:2]), c("Comp-FITC-A", "Comp-PerCP-Cy5-5-A"))
  expect_equal(unname(obj@parameterNames$fjStainNames[1:2]), c("CCR7", "CD16"))
  expect_true(is.na(obj@parameterNames$fjStainNames[["Comp-BUV395-A"]]))
  expect_false("EventNumberDP" %in% colnames(obj@inputCSV$X))
  expect_equal(ncol(obj@inputCSV$X), 18)
  expect_true(is.numeric(obj@inputCSV$X))
})

test_that("Phenograph is reported as not implemented", {
  obj <- new("CytoCrunch", sampleName = "Blood3k",
             inputCSVFilePath = system.file("extdata", "Blood3k.ExtNode.csv", package = "CytoCrunch"))
  expect_error(performClustering(obj, runID = "x", method = "Phenograph"), "not implemented")
})
