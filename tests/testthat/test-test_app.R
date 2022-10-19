# this script is to run all tests for test_app
library(shinytest2)

test_that("all tests for test_app pass", {
  skip_on_ci()
  app_dir <- file.path(system.file(package = "shinybreakpoint", "tests_helpers"),
                       "test_app")
  test_app(app_dir)
})


