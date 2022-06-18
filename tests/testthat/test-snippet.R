test_that("snippet returns error when outside RStudio or no opened file", {
  skip_if_not(interactive())
  skip_if(!is.null(rstudioapi::getActiveDocumentContext()$id)) # because we don't want to insert snippet to opened file during tests
  expect_error(snippet(), "Can't find opened file in RStudio.", fixed = TRUE)
})
