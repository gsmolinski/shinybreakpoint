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

test_that("restore_attrs_funs restores fun attrs", {
  server_old <- server
  body(server)[[1]] <- "b"
  expect_false(body(server_old) == body(server))

  restore_attrs_funs(path, rlang::current_env())
  expect_identical(body(server), body(server_old))
  expect_identical(attr(server, "srcref"), attr(server_old, "srcref"))
})

test_that("does_brakpoint_can_be_set returns TRUE if breakpoint would be set
          inside reactive and FALSE otherwise", {
  skip_if_not(interactive())
  expect_false(does_breakpoint_can_be_set(find_object(path, 34, envir)))
  expect_true(does_breakpoint_can_be_set(find_object(path, 35, envir)))
  expect_true(does_breakpoint_can_be_set(find_object(path, 58, envir)))
})

test_that("determine_line returns exact line where code will be added", {
  skip_if_not(interactive())
  path <- system.file("tests_helpers", "determine_line_test_helper.R", package = "shinybreakpoint")
  source(path, local = TRUE, keep.source = TRUE)

  expect_equal(determine_line(path, 15, environment(fun1), findLineNum(path, 15)[[1]]$at), 6)
  expect_equal(determine_line(path, 20, environment(fun1), findLineNum(path, 20)[[1]]$at), 20)
})

test_that("write_file_modified writes code to the correct lines", {
  skip_if_not(interactive())
  path <- system.file("tests_helpers", "determine_line_test_helper.R", package = "shinybreakpoint")
  source(path, local = TRUE, keep.source = TRUE)
  line <- determine_line(path, 15, environment(fun1), findLineNum(path, 15)[[1]]$at) # returns 6
  obj <- find_object(path, 15, environment(fun1))
  path_temp <- tempfile(paste0("DEBUGGING_", basename(path), "_____"), fileext = ".R")
  put_browser(obj, "....envirr")
  write_file_modified(path, line, obj$name, obj$envir, obj$at, path = path_temp)
  parsed <- eval(parse(path_temp)[[1]])

  expect_true(grepl("^assign\\(.+", readLines(path_temp)[[line]]))
  expect_equal(body(fun1), body(parsed))
})

test_that("set_attrs keeps original scrfile", {
  skip_if_not(interactive())
  path <- system.file("tests_helpers", "determine_line_test_helper.R", package = "shinybreakpoint")
  source(path, local = TRUE, keep.source = TRUE)
  line <- determine_line(path, 15, environment(fun1), findLineNum(path, 15)[[1]]$at) # returns 6
  obj <- find_object(path, 15, environment(fun1))
  put_browser(obj, "....envirr")
  original_srcfile <- utils::getSrcFilename(fun1)
  set_attrs(path, line, obj$name, obj$envir, obj$at, environment())

  expect_equal(utils::getSrcFilename(fun1), original_srcfile)
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
  obj$at[[length(obj$at)]] <- obj$at[[length(obj$at)]] + 3 # because 'browser()' is last line added
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
