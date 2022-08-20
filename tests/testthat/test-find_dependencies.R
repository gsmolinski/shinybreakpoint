reactlog_data_path <- system.file("tests_helpers", "reactlog_data.rds", package = "shinybreakpoint")
reactlog_data <- readRDS(reactlog_data_path)

test_that("'find_dependencies' returns correct source code if chosen id is input or not an input", {
  id <- "text_output"
  filenames_parse_data <- list(
    filename = c("app.R", "app.R", "mod1.R"),
    filename_full_path = c("a/app.R", "b/app.R", "c/mod1.R"),
    parse_data = list(
      data.frame(line = c(11, 12, 13, NA, 17, 18, NA),
                 src_code = c("a", "b", "c", NA, "d", "e", NA)),
      data.frame(line = c(20, 21, 22, NA, 24, 25, NA),
                 src_code = c("aa", "bb", "cc", NA, "dd", "ee", NA)),
      data.frame(line = c(1, NA, 2, NA),
                 src_code = c("aaa", NA, "bbb", NA))
    )
  )
  binded_filenames_parse_data <- prepare_filenames_parse_data(filenames_parse_data)
  labelled_observers <- data.frame(location_observer = c(1, 2),
                                  label = c("observe event with text", "observe with input text"),
                                  file = c("mod1.R", "mod1.R"))
  ids_dependency_df <- prepare_dependency_df_and_ids_data(reactlog_data, labelled_observers)
  reactlog_dependency_df <- ids_dependency_df$reactlog_dependency_df
  all_react_ids <- ids_dependency_df$all_react_ids
  ids_data <- ids_dependency_df$ids_data

  expected_when_id_is_input <- data.frame(filename_full_path = c(rep("b/app.R", 4), rep("a/app.R", 4)),
                                     filename = c(rep("app.R", 4), rep("app.R", 4)),
                                     line = c(20, 21, 22, NA, 11, 12, 13, NA),
                                     src_code = c("aa", "bb", "cc", NA_character_, "a", "b", "c", NA_character_))

  expect_identical(find_dependencies(id, binded_filenames_parse_data, reactlog_dependency_df, all_react_ids, ids_data),
                   expected_when_id_is_input)

  id <- "text"
  expect_identical(find_dependencies(id, binded_filenames_parse_data, reactlog_dependency_df, all_react_ids, ids_data),
                   expected_when_id_is_input)
})

test_that("'prepare_dependency_df_and_ids_data' returns list with correct result", {
  labelled_observers <- data.frame(location_observer = c(1, 2),
                                   label = c("observe event with text", "observe with input text"),
                                   file = c("app.R", "app.R"))

  expect_type(prepare_dependency_df_and_ids_data(reactlog_data, labelled_observers),
              "list")

  expected_ids_data <- data.frame(react_id = c("r3", "r4", "r5", "r6", "r7", "r8", "r9", "r1$text", "r1$button1", "r1$text1"),
                                  label_full = c("Theme Counter", "reactive_output_text", "reactive_output_digit", "output$text_output", "output$digit_output",
                                                 "observe event with text", "observe with input text", "input$text", "input$button1", "input$text1"),
                                  filename = c(NA_character_, rep("app.R", 6), rep(NA_character_, 3)),
                                  location = c(NA, 13, 17, 20, 24, 1, 2, rep(NA, 3)),
                                  is_input = c(rep(FALSE, 7), rep(TRUE, 3)),
                                  label = c("Theme Counter", "reactive_output_text", "reactive_output_digit", "text_output", "digit_output",
                                            "observe event with text", "observe with input text", "text", "button1", "text1"))
  expected_reactlog_dependency_df <- data.frame(react_id = c("r6", "r4", "r7", "r8", "r9"),
                                       depends_on_react_id = c("r4", "r1$text", "r5", "r1$button1", "r1$text1"),
                                       is_input = c(FALSE, TRUE, FALSE, TRUE, TRUE))
  expected_all_react_ids <- c("r6", "r4", "r7", "r8", "r9", "r1$text", "r5", "r1$button1", "r1$text1")

  expect_identical(prepare_dependency_df_and_ids_data(reactlog_data, labelled_observers)$ids_data,
                   expected_ids_data)
  expect_identical(prepare_dependency_df_and_ids_data(reactlog_data, labelled_observers)$reactlog_dependency_df,
                   expected_reactlog_dependency_df)
  expect_identical(prepare_dependency_df_and_ids_data(reactlog_data, labelled_observers)$all_react_ids,
                   expected_all_react_ids)

  labelled_observers <- NULL
  expected_ids_data$filename[6:7] <- NA_character_
  expected_ids_data$location[6:7] <- NA
  expected_ids_data$location <- as.integer(expected_ids_data$location)

  expect_identical(prepare_dependency_df_and_ids_data(reactlog_data, labelled_observers)$ids_data,
                   expected_ids_data)
  expect_identical(prepare_dependency_df_and_ids_data(reactlog_data, labelled_observers)$reactlog_dependency_df,
                   expected_reactlog_dependency_df)
  expect_identical(prepare_dependency_df_and_ids_data(reactlog_data, labelled_observers)$all_react_ids,
                   expected_all_react_ids)

})

test_that("'prepare_ids_data' prepares correct data if labelled_observers
          is not NULL or NULL", {
            labelled_observers <- data.frame(location_observer = c(1, 2),
                                             label = c("observe event with text", "observe with input text"),
                                             file = c("app.R", "app.R"))
            expected_data <- data.frame(react_id = c("r3", "r4", "r5", "r6", "r7", "r8", "r9", "r1$text", "r1$button1", "r1$text1"),
                                        label_full = c("Theme Counter", "reactive_output_text", "reactive_output_digit", "output$text_output", "output$digit_output",
                                                       "observe event with text", "observe with input text", "input$text", "input$button1", "input$text1"),
                                        filename = c(NA_character_, rep("app.R", 6), rep(NA_character_, 3)),
                                        location = c(NA, 13, 17, 20, 24, 1, 2, rep(NA, 3)),
                                        is_input = c(rep(FALSE, 7), rep(TRUE, 3)),
                                        label = c("Theme Counter", "reactive_output_text", "reactive_output_digit", "text_output", "digit_output",
                                                  "observe event with text", "observe with input text", "text", "button1", "text1"))
            expect_identical(prepare_ids_data(reactlog_data, labelled_observers),
                             expected_data)

            labelled_observers <- NULL
            expected_data$filename[6:7] <- NA_character_
            expected_data$location[6:7] <- NA
            expected_data$location <- as.integer(expected_data$location)
            expect_identical(prepare_ids_data(reactlog_data, labelled_observers),
                             expected_data)
          })

test_that("prepare_reactlog_dependency_df returns data.frame
          with dependencies react_ids", {
            expected_data <- data.frame(react_id = c("r6", "r4", "r7", "r8", "r9"),
                                        depends_on_react_id = c("r4", "r1$text", "r5", "r1$button1", "r1$text1"))
            expect_identical(prepare_reactlog_dependency_df(reactlog_data),
                             expected_data)
          })

test_that("'construct_dependency_graph' returns correct result
          if id is an input or not an input", {
            reactlog_dependency_df <- prepare_reactlog_dependency_df(reactlog_data)
            reactlog_dependency_df <- rbind(reactlog_dependency_df,
                                            data.frame(react_id = "r10",
                                                       depends_on_react_id = "r1$text"))
            reactlog_dependency_df$is_input <- c(FALSE, TRUE, FALSE, TRUE, TRUE, TRUE)
            all_react_ids <- unique(c(reactlog_dependency_df$react_id, reactlog_dependency_df$depends_on_react_id))

            expected_obj_input <- data.frame(graph = c(1, 1, 2, 3, 4, 1, 1, 2, 3, 4),
                                       react_id = c("r6", "r4", "r7", "r8", "r9", "r10", "r1$text", "r5", "r1$button1", "r1$text1"))
            expect_identical(construct_dependency_graph(reactlog_dependency_df, TRUE, all_react_ids),
                             expected_obj_input)

            expected_obj_not_input <- data.frame(graph = c(1, 2, 1, 2, 3, 4, 5, 6, 7, 8),
                                                 react_id = c("r6", "r7", "r4", "r5", "r8", "r9", "r10", "r1$text", "r1$button1", "r1$text1"))
            expect_identical(construct_dependency_graph(reactlog_dependency_df, FALSE, all_react_ids),
                             expected_obj_not_input)
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
