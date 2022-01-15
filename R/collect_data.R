#' Get Full Paths to Filenames, Filenames and Functions from Chosen Environments
#'
#' @param caller_env object returned by 'rlang::caller_env()', passed by exported function, i.e.
#' function used directly by user.
#'
#' @return data.frame with cols: filename_full_path, filename, fun_name
#' @importFrom magrittr %>%
#' @noRd
collect_filename_funs <- function(caller_env) {
  envs_funs <- lapply(rlang::env_parents(caller_env), names)
  envs_funs <- drop_envs_too_far(envs_funs)
  envs_funs <- unlist(envs_funs, use.names = FALSE)

  filename_srccode <- data.frame(filename_full_path = character(),
                                 filename = character(),
                                 fun_name = character())
  filename_srccode <- filename_srccode %>%
    dplyr::mutate(filename_full_path = vapply(envs_funs, get_filename, FUN.VALUE = character(1)),
                  filename = basename(filename_full_path),
                  fun_name = envs_funs)

  # keep only top-of-the-top funs from each file which have srcref
  filename_srccode <- as.data.frame(lapply(filename_srccode, rev))
  filename_srccode <- filename_srccode %>%
    dplyr::filter(!duplicated(filename_full_path) & !is.na(filename_full_path))

  filename_srccode
}

#' Remove Not Needed Environemnts
#'
#' @param envs_funs named list with functions from found environments
#'
#' @details
#' If list has at least one name 'namespace:' it means app is developed as package.
#' Then 'env_parents()' from 'collect_filename_funs' function will return other
#' packages as well and after them global environment. We don't need them, because
#' there won't be, developed by user, reactive functions.
#'
#' If list doesn't have at least one name 'namespace:' it means app is not developed
#' as a package. Then we just need to remove global environment, because there won't be
#' reactive functions.
#'
#' @return list without global environment and without environments from other packages
#' @noRd
drop_envs_too_far <- function(envs_funs) {
  if (any(grepl("namespace:", names(envs_funs)))) {
    first_namespace <- grep("namespace:", names(envs_funs))[[1]]
    envs_funs <- envs_funs[1:first_namespace]
  } else {
    envs_funs[["global"]] <- NULL
  }
  envs_funs
}

#' Get Filename with Full Path for Function
#'
#' @param one_envs_funs one element of envs_funs
#'
#' @return full path for function if functon was sourced from file,
#' otherwise NA_character_
#' @noRd
get_filename <- function(one_envs_funs) {
  filename <- attr(attr(get(one_envs_funs),"srcref"),"srcfile")$filename
  if (is.null(filename)) {
    filename <- NA_character_
  }
  filename
}
