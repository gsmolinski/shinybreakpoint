reactlog_data_path <- system.file("tests_helpers", "reactlog_data.rds", package = "shinybreakpoint")
reactlog_data <- readRDS(reactlog_data_path)

test_that("prepare_filenames_parse_data adds correct cols, binds parse_data
          and makes groups for reactive blocks", {
  data_example <- list(
    filename = c("test1", "test2"),
    filename_full_path = c("a/test1", "b/test2"),
    parse_data = list(
      data.frame(line = c(1, 2, 3, NA, 4, 5, NA),
                 src_code = c("a", "b", "c", NA, "a", "b", NA)),
      data.frame(line = c(2, NA, 3, NA, 4, NA),
                 src_code = c("a", NA, "c", NA, "d", NA))
    )
  )

  data_expected <- data.frame(
    line = c(1, 2, 3, NA, 4, 5, NA, 2, NA, 3, NA, 4, NA),
    src_code = c("a", "b", "c", NA, "a", "b", NA, "a", NA, "c", NA, "d", NA),
    filename = c(rep("test1", 7), rep("test2", 6)),
    filename_full_path = c(rep("a/test1", 7), rep("b/test2", 6)),
    each_reactive = c(rep(1L, 4), rep(2L, 3), rep(3L, 2), rep(4L, 2), rep(5L, 2))
  )

  expect_identical(prepare_filenames_parse_data(data_example),
                   data_expected)
})

test_that("prepare_reactlog_dependency_df returns data.frame
          with dependencies react_ids", {
  expected_data <- data.frame(react_id = c("r6", "r4", "r7", "r8", "r9"),
                              depends_on_react_id = c("r4", "r1$text", "r5", "r1$button1", "r1$text1"))
  expect_identical(prepare_reactlog_dependency_df(reactlog_data),
                   expected_data)
})


