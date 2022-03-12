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

server <- function(id) {
  shinybreakpointServer(id = "shinybreakpoint")
}

test_that("'which_file' reactive returns indice where is file", {
  testServer(server, {
    filenames_src_code_envirs <- prepare_src_code(g)
    session$setInputs(file = filenames_src_code_envirs$filenames_parse_data$filename_full_path)
    expect_equal(which_file(), 1)
  })
})

test_that("'selected_line' reactive returns not truthy object if no line selected
          or NA (empty line) selected and truthy otherwise", {
  testServer(server, {
    filenames_src_code_envirs <- prepare_src_code(g)
    session$setInputs(file = filenames_src_code_envirs$filenames_parse_data$filename_full_path,
                      src_code__reactable__selected = NULL)
    expect_false(isTruthy(selected_line()))
    session$setInputs(src_code__reactable__selected = 22)
    expect_false(isTruthy(selected_line()))
    session$setInputs(src_code__reactable__selected = 14)
    expect_true(isTruthy(selected_line()))
  })
})
# don't know why it has to be repeated, but otherwise don't work
server <- function(id) {
  shinybreakpointServer(id = "shinybreakpoint")
}

test_that("'object' reactive returns error (do not run) if NA (empty line) selected
          and truthy otherwise", {
  skip_if_not(interactive())
  testServer(server, {
    filenames_src_code_envirs <- prepare_src_code(g)
    session$setInputs(file = filenames_src_code_envirs$filenames_parse_data$filename_full_path,
                      src_code__reactable__selected = 22)
    expect_error(req(object()))
    session$setInputs(src_code__reactable__selected = 14)
    expect_true(isTruthy(object()))
  })
})
# don't know why it has to be repeated, but otherwise don't work
server <- function(id) {
  shinybreakpointServer(id = "shinybreakpoint")
}

test_that("'breakpoint_can_be_set' reactive returns FALSE (not truthy) if not possible
          to put browser and TRUE (truthy) otherwise", {
  skip_if_not(interactive())
  testServer(server, {
    filenames_src_code_envirs <- prepare_src_code(g)
    session$setInputs(file = filenames_src_code_envirs$filenames_parse_data$filename_full_path,
                      src_code__reactable__selected = 23)
    expect_false(isTruthy(breakpoint_can_be_set()))
    session$setInputs(src_code__reactable__selected = 14)
    expect_true(isTruthy(breakpoint_can_be_set()))
  })
})
