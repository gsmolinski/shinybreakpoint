path <- system.file("tests_helpers", "server_fun_with_srcref.R", package = "shinybreakpoint")
source(path, local = TRUE, keep.source = TRUE)
parse_data_srcref <- utils::getParseData(server, includeText = NA)

test_that("'prepare_src_code' returns list if srcref", {
  skip_if_not(interactive())
  e <- new.env(parent = rlang::pkg_env("shinybreakpoint"))
  expect_type(prepare_src_code(e), "list")
})

test_that("'prepare_src_code' returns NULL if no srcref", {
  env_global_as_parent <- new.env(parent = .GlobalEnv)
  expect_equal(prepare_src_code(env_global_as_parent), NULL)
})

test_that("'find_direct_parent_id_with_reactive' finds only reactives and only top id", {
  expect_equal(find_direct_parent_id_with_reactive(parse_data_srcref)$id,
                   c(28, 76, 130, 268, 351, 422, 388, 409, 448, 509, 499))
})

test_that("'remove_nested_reactives' removes nested reactives", {
  only_reactives_not_nested <- find_direct_parent_id_with_reactive(parse_data_srcref)
  expect_equal(remove_nested_reactives(only_reactives_not_nested)$id,
                   c(28, 76, 130, 268, 351, 422, 448, 509))
})

test_that("filename is keep before using 'retrieve_src_code'", {
  find_direct_result <- find_direct_parent_id_with_reactive(parse_data_srcref)
  filename <- attr(find_direct_result, "srcfile")$filename
  expect_equal(filename, attr(attr(server, "srcref"), "srcfile")$filename)

  remove_nested_result <- remove_nested_reactives(find_direct_result)
  filename <- attr(remove_nested_result, "srcfile")$filename
  expect_equal(filename, attr(attr(server, "srcref"), "srcfile")$filename)
})

test_that("'retrieve_src_code' returns data.frame with lines and src code", {
  skip_if_not(interactive())
  expected <- structure(list(line = c(2L, 3L, 4L, NA, 9L, 10L, 11L, NA, 17L,
                                      18L, 19L, NA, 34L, 35L, 36L, NA, 37L, 38L, 39L, 40L, 41L, NA,
                                      43L, 44L, 45L, 46L, 47L, 48L, 49L, 50L, 51L, NA, 52L, 53L, 54L,
                                      NA, 55L, 56L, 57L, 58L, 59L, 60L), src_code = c("shiny::observe({",
                                                                                      "    \"x\"", "  })", NA, "observe({", "      \"o\"", "    })",
                                                                                      NA, "observe({", "      \"!\"", "    })", NA, "test3 <- reactive({",
                                                                                      "    \"a\"", "  })", NA, "observe({", "    assign(\"parse_data\", getParseData(environment(server)$server, includeText = NA),",
                                                                                      "           envir = new.env())", "    test3()", "  })", NA,
                                                                                      "observeEvent(input$test1, {", "    observe({", "      \"c\"",
                                                                                      "    })", "    observe({", "      \"d\"", "    })", "", "  })",
                                                                                      NA, "reactive({", "    \"e\"", "  }) -> test4", NA, "eventReactive(input$test1, {",
                                                                                      "    output$test2 <- renderPrint({", "      test4()", "      \"Do not nest reactives.\"",
                                                                                      "    })", "  })")), class = "data.frame", row.names = c(NA, -42L
                                                                                      ))

  only_reactives <- find_direct_parent_id_with_reactive(parse_data_srcref)
  only_reactives_not_nested <- remove_nested_reactives(only_reactives)

  expect_equal(retrieve_src_code(only_reactives_not_nested),
                   expected)
})
