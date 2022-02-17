test_that("has_assignment returns TRUE only if expr has assignment", {
  expect_true(has_assignment(parse(text = "x <- function() {}")[[1]]))
  expect_true(has_assignment(parse(text = "x <<- function() {}")[[1]]))
  expect_true(has_assignment(parse(text = "x = function() {}")[[1]]))
  expect_true(has_assignment(parse(text = "assign('x', function() {})")[[1]]))
  expect_false(has_assignment(parse(text = "function() {}")[[1]]))
})

test_that("is_named_fun returns TRUE only if expr is function and is named", {
  expect_true(is_named_fun(parse(text = "x <- function() {}")[[1]]))
  expect_false(is_named_fun(parse(text = "function() {}")[[1]]))
  expect_false(is_named_fun(parse(text = "x <- list()")[[1]]))
  expect_false(is_named_fun(parse(text = "x <- shiny::observe()")[[1]]))
})

test_that("is_reactive_context returns TRUE if is inside reactive context", {
  fun <- function() {
    observe({
    })

    shiny::observeEvent(input$input, {
    })

    observe()
  }

  expect_true(is_reactive_context(body(fun)[[2]]))
  expect_true(is_reactive_context(body(fun)[[3]]))
  expect_true(is_reactive_context(body(fun)[[4]]))
})
