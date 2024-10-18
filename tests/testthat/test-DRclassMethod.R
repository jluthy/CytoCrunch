test_that("DimRedClass method and parameters are set correctly", {
  # Example data
  reducedData <- matrix(rnorm(100), nrow=50, ncol=2)
  method <- "tSNE"

  # Create DimRedClass object
  dimRedObj <- new("DimRedClass", result = list(reducedData = reducedData), method = method)

  # Check method
  expect_equal(dimRedObj@method, "tSNE")
})
