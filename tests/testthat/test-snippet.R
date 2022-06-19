test_that("snippet returns error when outside RStudio or no opened file", {
  skip_if_not(is.null(tryCatch(rstudioapi::getSourceEditorContext()$id, error = function() NULL)))
  expect_error(snippet(), "Can't find opened file in RStudio.", fixed = TRUE)
})
