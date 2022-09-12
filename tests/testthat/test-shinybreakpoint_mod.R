path <- system.file("tests_helpers", "server_fun_with_srcref.R", package = "shinybreakpoint")
source(path, local = TRUE, keep.source = TRUE)
e <- new.env(parent = .GlobalEnv)
e$server_test <- server
g <- new.env(parent = e)

reactlog_data_path <- system.file("tests_helpers", "reactlog_data.rds", package = "shinybreakpoint")
reactlog_data <- readRDS(reactlog_data_path)

test_that("js code in shinybreakpointUI is correct", {
  val <- shinybreakpointUI("test")
  expect_equal(
  val$children[[1]], tags$script(HTML(
  '  document.addEventListener("keydown", function(e) {
   Shiny.setInputValue("test-key_pressed", e.key, {priority: "event"});
  });'
))) # assumes shiny::NS(id) behavior is to add '{id}-' before inputId
})

test_that("'get_src_editor_file' returns first value when file out of list", {
  expect_equal(get_src_editor_file(c("path/file1.R", "path/file2.R")), "path/file1.R")
})

test_that("'validate_id' returns error if reactlog_data is NULL or labels
          in ids_data are duplicated", {
            expect_error(validate_id("id", NULL, NULL))
            ids_data <- data.frame(label = c("label1", "label1", "label2", "label3", "label3"))
            expect_error(validate_id("id", data.frame(a = 1), ids_data),
                         regexp = "These Ids or labels are duplicated: label1, label3. Use only unique Ids and labels.")
          })

test_that("'create_UI' displays simple info with no source code
          if no source code found with reactive objects", {
            expected <- tags$div(class = "no-file",
                                 tags$div(class = "circle-div",
                                          tags$div(class = "circle")),
                                 tags$p("There is nothing to see here"))
            expect_identical(create_UI(NULL, NULL, NULL),
                             expected)
            df_no_row <- data.frame(a = 1,
                                    b = 2)
            df_no_row <- df_no_row[FALSE, ]
            expect_identical(create_UI(NULL, df_no_row, NULL),
                             expected)
          })

test_that("'get_dependencies_set_names' returns named list", {
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
  labelled_reactive_objects <- data.frame(location_object = c(1, 2),
                                          label = c("observe event with text", "observe with input text"),
                                          file = c("mod1.R", "mod1.R"))
  ids_dependency_df <- prepare_dependency_df_and_ids_data(reactlog_data, labelled_reactive_objects)
  reactlog_dependency_df <- ids_dependency_df$reactlog_dependency_df
  ids_data <- ids_dependency_df$ids_data
  result <- get_dependencies_set_names(id, find_dependencies, binded_filenames_parse_data, reactlog_dependency_df, ids_data)

  expect_type(result, "list")
  expect_named(result, "text_output")
})

server <- function(id = "shinybreakpoint") {
  shinybreakpointServer(id = id)
}

test_that("'get_files' returns named list", {
  testServer(server, {
    filenames_src_code_envirs <- prepare_src_code(g)
    expect_type(get_files(), "list")
    expect_named(get_files())
  })
})


