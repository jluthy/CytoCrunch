test_that("CytoCrunch Class creation works", {
  csv_path1 <- system.file("extdata", "Blood3k.ExtNode.csv", package = "CytoCrunch")
  flowData2 <- new("CytoCrunch",
                   sampleName = "Blood3k",
                   inputCSVFilePath = csv_path1
  )
  expect_equal(flowData2@sampleName, "Blood3k")
  expect_true(is.matrix(flowData2@inputCSV$X))
  expect_true(is.matrix(flowData2@inputCSV$eventNumbers))
})
