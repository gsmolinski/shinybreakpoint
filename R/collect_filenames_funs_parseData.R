#' Get Filenames, Functions Names and Parse Data
#'
#' @param caller_env object returned by 'rlang::caller_env()', passed by exported function, i.e.
#' function used directly by user.
#'
#' @return data.frame with cols: filename_full_path, filename, fun_name, parse_data.
#' @details
#' It keps only these functions from each source file, which are top functions, i.e. it omits
#' nested functions. It is because we want to use 'body()' later, so we will have access
#' to these nested functions.
#' @importFrom magrittr %>%
#' @noRd
collect_filenames_funs_parseData <- function(caller_env) {
  envs_funs <- lapply(rlang::env_parents(caller_env), names)
  envs_funs <- drop_envs_too_far(envs_funs)
  names(envs_funs) <- paste0(1:length(envs_funs), "_")
  envs_funs <- unlist(envs_funs, use.names = TRUE)
  names(envs_funs) <- gsub("_\\d*", replacement = "", x = names(envs_funs)) # env depth level

  filenames_funs <- data.frame(
    filename_full_path = vapply(envs_funs, get_filename, FUN.VALUE = character(1),
                                caller_env = caller_env)
  )
  filenames_funs <- filenames_funs %>%
    dplyr::mutate(filename = basename(filename_full_path),
                  fun_name = envs_funs,
                  env_depth = as.integer(names(envs_funs))) %>%
    dplyr::filter(!is.na(filename_full_path)) %>%
    dplyr::group_by(filename_full_path) %>%
    dplyr::slice_max(env_depth) %>% # keep only unnested functions
    dplyr::ungroup() %>%
    dplyr::select(-env_depth) %>%
    dplyr::mutate(parse_data = lapply(envs_funs, get_parse_data, env_caller = env_caller))

  filenames_funs
}

#' Remove Not Needed Environments
#'
#' @param envs_funs named list with functions from found environments.
#'
#' @details
#' If list has at least one name 'namespace:' it means app is developed as package.
#' Then 'env_parents()' from 'collect_filenames_funs_parseData' function will return other
#' packages as well and after them global environment. We don't need them, because
#' there won't be, developed by user, reactive functions.
#'
#' If list doesn't have at least one name 'namespace:' it means app is not developed
#' as a package. Then we just need to remove global environment, because there won't be
#' reactive functions.
#'
#' @return list without global environment and without environments from other packages.
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
#' @param one_envs_funs one element of envs_funs.
#' @param caller_env object returned by 'rlang::caller_env()', passed by exported function, i.e.
#' function used directly by user.
#'
#' @return full path for function if functon was sourced from file,
#' otherwise NA_character_.
#' @noRd
get_filename <- function(one_envs_funs, caller_env) {
  filename <- attr(attr(get(one_envs_funs, envir = caller_env),"srcref"),"srcfile")$filename
  if (is.null(filename)) {
    filename <- NA_character_
  }
  filename
}

#' Use 'getParseData' on Object
#'
#' @param one_envs_funs one element of envs_funs.
#' @param caller_env object returned by 'rlang::caller_env()', passed by exported function, i.e.
#' function used directly by user.
#'
#' @return data.frame returned by 'getParseData()'.
#' @noRd
get_parse_data <- function(one_envs_funs, caller_env) {
  parse_data <- getParseData(get(one_envs_funs, envir = caller_env), includeText = NA)
  parse_data
}
