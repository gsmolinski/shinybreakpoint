#' Get Filenames and Parse Data
#'
#' @param caller_env object returned by 'rlang::caller_env()', passed by exported function, i.e.
#' function used directly by user.
#'
#' @return data.frame with cols: filename_full_path, filename, parse_data.
#' @importFrom magrittr %>%
#' @noRd
collect_filenames_parse_data <- function(caller_env) {
  envs_objs <- find_objects_in_envirs(caller_env)
  envs_objs <- drop_envs_too_far(envs_objs)
  envs_objs <- unlist(envs_objs, use.names = FALSE)

  filenames_parse_data <- data.frame(
    filename_full_path = vapply(envs_objs, get_filename, FUN.VALUE = character(1),
                                caller_env = caller_env),
    obj_name = envs_objs
  )
  filenames_parse_data <- filenames_parse_data %>%
    dplyr::filter(!is.na(filename_full_path) & !duplicated(filename_full_path)) %>%
    dplyr::mutate(filename = basename(filename_full_path),
                  parse_data = lapply(obj_name, get_parse_data, caller_env = caller_env)) %>%
    dplyr::select(-obj_name)

  filenames_parse_data
}

#' Get Names of Objects from Current and Parent Environments
#'
#' @param caller_env object returned by 'rlang::caller_env()', passed by exported function, i.e.
#' function used directly by user.
#'
#' @return named list if environment has name with names (character type) of objects belong
#' to each environment.
#' @details
#' Function starts from current environment, which is a caller environment of exported function,
#' i.e. 'shinybrowserServer' and go through the parent environments, i.e. environments returned
#' by 'rlang::env_parents()' function.
#' @noRd
find_objects_in_envirs <- function(caller_env) {
  lapply(rlang::env_parents(caller_env), names)
}

#' Remove Not Needed Environments
#'
#' @param envs_objs named list with objects from found environments.
#'
#' @details
#' If list has at least one name 'namespace:' it means app is developed as package.
#' Then 'env_parents()' from 'find_objects_in_envirs' function will return other
#' packages as well and after them global environment. We don't need them, because
#' there won't be, developed by user, reactive functions.
#'
#' If list doesn't have at least one name 'namespace:' it means app is not developed
#' as a package. Then we just need to remove global environment, because there won't be
#' reactive functions.
#'
#' @return list without global environment and without environments from other packages.
#' @noRd
drop_envs_too_far <- function(envs_objs) {
  if (any(grepl("namespace:", names(envs_objs)))) {
    first_namespace <- grep("namespace:", names(envs_objs))[[1]]
    envs_objs <- envs_objs[1:first_namespace]
  } else {
    envs_objs[["global"]] <- NULL
  }
  envs_objs
}

#' Get Filename with Full Path for Object
#'
#' @param one_envs_objs one element of envs_objs
#' @param caller_env object returned by 'rlang::caller_env()', passed by exported function, i.e.
#' function used directly by user.
#'
#' @return full path for object if object was sourced from file,
#' otherwise NA_character_.
#' @noRd
get_filename <- function(one_envs_objs, caller_env) {
  filename <- attr(attr(get(one_envs_objs, envir = caller_env),"srcref"),"srcfile")$filename
  if (is.null(filename)) {
    filename <- NA_character_
  }
  filename
}

#' Use 'getParseData' on Object
#'
#' @param one_obj_name one element of obj_name column.
#' @param caller_env object returned by 'rlang::caller_env()', passed by exported function, i.e.
#' function used directly by user.
#'
#' @return data.frame returned by 'getParseData()'.
#' @noRd
get_parse_data <- function(one_obj_name, caller_env) {
  parse_data <- getParseData(get(one_obj_name, envir = caller_env), includeText = NA)
  parse_data
}
