test_that("snippet returns error when outside RStudio or no opened file", {
  expect_error(snippet(), "Can't find opened file in RStudio.", fixed = TRUE)
})
