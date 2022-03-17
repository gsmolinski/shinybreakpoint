path <- system.file("tests_helpers", "server_fun_with_srcref.R", package = "shinybreakpoint")
source(path, local = TRUE, keep.source = TRUE)
envir <- environment(server)

test_that("find_object returns NULL if nothing has been found", {
  expect_null(find_object(path, 0, envir))
})

test_that("find_object returns list if obj found with adequate type of elements", {
  skip_if_not(interactive())
  obj <- find_object(path, 35, envir)
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
  expect_false(does_breakpoint_can_be_set(find_object(path, 34, envir)))
  expect_true(does_breakpoint_can_be_set(find_object(path, 35, envir)))
  expect_true(does_breakpoint_can_be_set(find_object(path, 58, envir)))
})

test_that("put_browser adds correct number of exprs to the body of fun before chosen line", {
  skip_if_not(interactive())
  obj <- find_object(path, 35, envir)
  location <- determine_location(obj$at)
  obj_orig_body_len <- length(body(obj$name)[[obj$at[-length(obj$at)]]])
  varName <- "....envirr"
  # because of error from shiny::getDefaultReactiveDomain$reload() if not inside server, use 'try'
  try(put_browser(obj, varName), silent = TRUE)
  obj_body_len <- length(body(obj$name)[[obj$at[-length(obj$at)]]])

  expect_equal(obj_body_len, obj_orig_body_len + length(get_code_to_put(envir, obj$name, location$at, location$location_in_fun,
                                                                        "....envirr")))
  expect_true(body(obj$name)[[obj$at]] == quote(browser()))
})

test_that("put_browser adds code which will remove added code", {
  skip_if_not(interactive())
  # we need to put 'browser' directly inside 'server', not nested in other obj, because
  # otherwise 'server()' won't make added code to run and thus code won't be removed
  obj <- find_object(path, 34, envir)
  orig_body <- body(obj$name)
  varName <- "....envirr"
  # because of error from shiny::getDefaultReactiveDomain$reload() if not inside server, use 'try'
  try(put_browser(obj, varName), silent = TRUE)
  try(server(NULL, NULL, NULL), silent = TRUE)

  expect_identical(body(obj$name), orig_body)
})

test_that("get_envir returns environment which has the same label as searched label", {
  e <- rlang::current_env()
  expect_identical(get_envir(rlang::env_label(e), rlang::current_env()), e)
})

test_that("remove_body_expr constructs correct indices to remove", {
  skip_if_not(interactive())
  obj <- find_object(path, 35, envir)
  location <- determine_location(obj$at)
  var_sym <- as.symbol("....envirr")
  expr <- remove_body_expr(obj$name, location$at, location$location_in_fun, var_sym)
  expr <- regmatches(expr, regexpr("-c.+)", expr))
  expr <- gsub("-", "", expr)
  to_test <- eval(str2lang(expr))
  expect_equal(to_test, location$location_in_fun + seq_len(length(get_code_to_put(envir,
                                                                                  obj$name,
                                                                                  location$at,
                                                                                  location$location_in_fun,
                                                                                  var_sym))))
})
