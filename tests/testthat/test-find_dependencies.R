reactlog_data_path <- system.file("tests_helpers", "reactlog_data.rds", package = "shinybreakpoint")
reactlog_data <- readRDS(reactlog_data_path)

test_that("'construct_dependency_graph' returns correct result
          if id is an input", {
            reactlog_dependency_df <- prepare_reactlog_dependency_df(reactlog_data)
            reactlog_dependency_df <- rbind(reactlog_dependency_df,
                                            data.frame(react_id = "r10",
                                                       depends_on_react_id = "r1$text"))
            reactlog_dependency_df$is_input <- c(FALSE, TRUE, FALSE, TRUE, TRUE, TRUE)
            all_react_ids <- unique(c(reactlog_dependency_df$react_id, reactlog_dependency_df$depends_on_react_id))

            expected_obj <- data.frame(graph = c(1, 1, 2, 3, 4, 1, 1, 2, 3, 4),
                                       react_id = c("r6", "r4", "r7", "r8", "r9", "r10", "r1$text", "r5", "r1$button1", "r1$text1"))

            expect_identical(construct_dependency_graph(reactlog_dependency_df, TRUE, all_react_ids),
                             expected_obj)
          })

test_that("'construct_dependency_graph' returns correct result
          if id is not an input", {
            reactlog_dependency_df <- prepare_reactlog_dependency_df(reactlog_data)
            reactlog_dependency_df <- rbind(reactlog_dependency_df,
                                            data.frame(react_id = "r10",
                                                       depends_on_react_id = "r1$text"))
            reactlog_dependency_df$is_input <- c(FALSE, TRUE, FALSE, TRUE, TRUE, TRUE)
            all_react_ids <- unique(c(reactlog_dependency_df$react_id, reactlog_dependency_df$depends_on_react_id))

            expected_obj <- data.frame(graph = c(1, 2, 1, 2, 3, 4, 5, 6, 7, 8),
                                       react_id = c("r6", "r7", "r4", "r5", "r8", "r9", "r10", "r1$text", "r1$button1", "r1$text1"))
            expect_identical(construct_dependency_graph(reactlog_dependency_df, FALSE, all_react_ids),
                             expected_obj)
          })

test_that("prepare_reactlog_dependency_df returns data.frame
          with dependencies react_ids", {
            expected_data <- data.frame(react_id = c("r6", "r4", "r7", "r8", "r9"),
                                        depends_on_react_id = c("r4", "r1$text", "r5", "r1$button1", "r1$text1"))
            expect_identical(prepare_reactlog_dependency_df(reactlog_data),
                             expected_data)
          })

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
