test_that("collect_filenames_parse_data returns NULL if no srcfile", {
  env_global_as_parent <- new.env(parent = .GlobalEnv)
  env_global_as_parent$a <- 1
  expect_equal(collect_filenames_parse_data(env_global_as_parent), NULL)
})

test_that("collect_filenames_parse_data returns list if srcfile", {
  skip_if_not(interactive())
  e <- new.env(parent = rlang::pkg_env("shinybreakpoint"))
  expect_type(collect_filenames_parse_data(e), "list")
})

test_that("drop_envs_too_far returns list", {
  expect_type(drop_envs_too_far(rlang::env_parents()), "list")
})

test_that("drop_envs_too_far returns only one namespace / package if any", {
  names_left <- names(drop_envs_too_far(rlang::env_parents(rlang::caller_env())))
  names_left <- grep("namespace:|package:", names_left)
  if (length(names_left) > 0) {
    expect_length(names_left, 1)
  }
})

test_that("drop_envs_too_far does not return global env", {
  expect_true(!any(grepl("global",
                         names(drop_envs_too_far(rlang::env_parents(rlang::caller_env()))))))
})

test_that("get_filenames_parse_data returns NULL if no objects in env", {
    expect_equal(get_filenames_parse_data(new.env()), NULL)
  })

test_that("get_filenames_parse_data returns no rows if no srcfile", {
  e <- new.env()
  e$a <- 2
  df <- data.frame(filename_full_path = character())
  df$parse_data = list()
  df$env_label = character()
  df <- df[FALSE, ]
  rownames(df) <- character()
  expect_equal(get_filenames_parse_data(e), df)
})

test_that("get_filenames_parse_data returns data if at least one srcfile", {
  skip_if_not(interactive())
  result <- get_filenames_parse_data(rlang::pkg_env("shinybreakpoint"))
  expect_true(!is.null(result) && nrow(result) > 0)
})

test_that("get_filenames_parse_data returns parse data for each path", {
  skip_if_not(interactive())
  filenames_parse_data <- get_filenames_parse_data(rlang::pkg_env("shinybreakpoint"))
  has_parse_data <- unlist(lapply(filenames_parse_data$parse_data,
                                  function(x) !is.null(x) && nrow(x) > 0))
  expect_true(!is.null(has_parse_data) && all(has_parse_data))
})

test_that("get_filename returns NA_character_ if no srcfile", {
  e <- new.env()
  e$a <- 2
  expect_equal(get_filename("a", envir = e), NA_character_)
})
