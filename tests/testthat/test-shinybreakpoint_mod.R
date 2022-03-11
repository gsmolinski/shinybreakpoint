path <- system.file("tests_helpers", "server_fun_with_srcref.R", package = "shinybreakpoint")
source(path, local = TRUE, keep.source = TRUE)
e <- new.env(parent = .GlobalEnv)
e$server_test <- server
g <- new.env(parent = e)

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

server <- function(id = "shinybreakpoint") {
  shinybreakpointServer()
}

test_that("'which_file' returns indice where is file", {
  testServer(server, {
    filenames_src_code_envirs <- prepare_src_code(g)
    session$setInputs(file = filenames_src_code_envirs$filenames_parse_data$filename_full_path)
    expect_equal(which_file(), 1)
  })
})

test_that("'selected_line' returns not truthy object if no line selected", {
  testServer(server, {
    filenames_src_code_envirs <- prepare_src_code(g)
    session$setInputs(file = filenames_src_code_envirs$filenames_parse_data$filename_full_path,
                      src_code__reactable__selected = NULL)
    expect_false(isTruthy(selected_line()))
  })
})
