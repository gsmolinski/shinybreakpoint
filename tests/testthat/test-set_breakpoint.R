path <- system.file("tests_helpers", "server_fun_with_srcref.R", package = "shinybreakpoint")
# adds function 'server' to environment
source(path, local = TRUE, keep.source = TRUE)
envir <- environment(server)

test_that("find_object returns NULL if nothing has been found", {
  # use line 0 to ensure nothing will be find in file
  expect_null(find_object(path, 0, envir))
})

test_that("find_object returns list if obj found with adequate type of elements", {
  skip_if_not(interactive())
  obj <- find_object(path, 2, envir)
  expect_type(obj, "list")
  expect_type(obj$name, "character")
  expect_type(obj$at, "integer")
  expect_type(obj$env, "environment")
})

test_that("restore_body_funs restores fun body", {
  server_old <- server
  body(server)[[1]] <- quote("b")
  expect_false(body(server_old) == body(server))

  restore_body_funs(path, rlang::current_env())
  expect_identical(body(server), body(server_old))
})

test_that("does_brakpoint_can_be_set returns TRUE if breakpoint would be set
          inside reactive and FALSE otherwise", {
  skip_if_not(interactive())
  expect_false(does_breakpoint_can_be_set(find_object(path, 10, envir)))
  expect_true(does_breakpoint_can_be_set(find_object(path, 3, envir)))
  expect_true(does_breakpoint_can_be_set(find_object(path, 12, envir)))
})

test_that("put_browser adds 5 exprs to the body of fun before chosen line", {
  skip_if_not(interactive())
  obj <- find_object(path, 3, envir)
  server_orig_body_len <- length(body(server)[[obj$at[-length(obj$at)]]])
  # because of error from shiny::getDefaultReactiveDomain$reload() if not inside server, use 'try'
  try(put_browser(obj), silent = TRUE)
  server_body_len <- length(body(server)[[obj$at[-length(obj$at)]]])

  expect_equal(server_body_len, server_orig_body_len + 5)
  expect_true(body(server)[[obj$at]] == quote(browser()))
})

test_that("put_browser adds code which will remove added code", {
  skip_if_not(interactive())
  # we need to put 'browser' directly inside 'server', not nested in other obj, because
  # otherwise 'server()' won't make added code to run and thus code won't be removed
  obj <- find_object(path, 2, envir)
  orig_body <- body(server)
  # because of error from shiny::getDefaultReactiveDomain$reload() if not inside server, use 'try'
  try(put_browser(obj), silent = TRUE)
  try(server(NULL, NULL, NULL), silent = TRUE)

  expect_identical(body(server), orig_body)
})

test_that("get_envir returns environment which has the same label as searched label", {
  e <- rlang::current_env()
  expect_identical(get_envir(rlang::env_label(e), rlang::current_env()), e)
})

