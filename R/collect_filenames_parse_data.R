#' Colect Filenames and Parse Data
#'
#' It collects results from 'get_filenames_parse_data_env' function.
#'
#' @param caller_env object returned by 'rlang::caller_env()', passed by exported function, i.e.
#' function used directly by user.
#'
#' @return data.frame with cols: filename_full_path, filename, parse_data.
#' @importFrom magrittr %>%
#' @noRd
collect_filenames_parse_data <- function(caller_env) {
  envirs <- rlang::env_parents(caller_env)
  envirs <- drop_envs_too_far(envirs)
  filenames_parse_data <- lapply(envirs, get_filenames_parse_data)
  filenames_parse_data <- dplyr::bind_rows(filenames_parse_data)

  filenames_parse_data <- filenames_parse_data %>%
    dplyr::filter(!duplicated(filename_full_path)) %>%
    dplyr::mutate(filename = basename(filename_full_path)) %>%
    dplyr::relocate(filename, .before = parse_data)

 filenames_parse_data
}

#' Remove Not Needed Environments
#'
#' @param envs_objs named list with found environments.
#'
#' @details
#' If list has at least one name 'namespace:' it means app is developed as package.
#' Then 'env_parents()' function will return other
#' packages as well and after them global environment. We don't need them, because
#' there won't be, developed by user, reactive functions.
#'
#' If list doesn't have at least one name 'namespace:' it means app is not developed
#' as a package. Then we just need to remove global environment, because there won't be
#' reactive functions.
#'
#' @return list without global environment and without environments from other packages.
#' @noRd
drop_envs_too_far <- function(envirs) {
  if (any(grepl("namespace:", names(envirs)))) {
    first_namespace <- grep("namespace:", names(envirs))[[1]]
    envirs <- envirs[1:first_namespace]
  } else {
    envirs[["global"]] <- NULL
  }
  envirs
}

#' Get Full Path to File and Parse Data for Object
#'
#' @param envir each environment returned by 'rlang::env_parents()',
#' passed by 'collect_filenames_parse_data' function.
#'
#' @return data.frame with cols: filename_full_path, parse_data.
#' @importFrom magrittr %>%
#' @noRd
get_filenames_parse_data <- function(envir) {
  env_objs <- names(envir)

  if (length(env_objs) > 0) {
    filenames_parse_data <- data.frame(
      filename_full_path = vapply(env_objs, get_filename, FUN.VALUE = character(1),
                                  envir = envir),
      obj_name = env_objs
    )

    filenames_parse_data <- filenames_parse_data %>%
      dplyr::filter(!is.na(filename_full_path) & !duplicated(filename_full_path)) %>%
      dplyr::mutate(parse_data = lapply(obj_name, get_parse_data, envir = envir))
      dplyr::select(-obj_name)

      filenames_parse_data
  } else {
    NULL
  }
}

#' Get Filename with Full Path for Object
#'
#' @param one_envs_objs one element of envs_objs
#' @param envir environment where object lives, passed to 'get'.
#'
#' @return full path for object if object was sourced from file,
#' otherwise NA_character_.
#' @noRd
get_filename <- function(one_envs_objs, envir) {
  filename <- attr(attr(get(one_envs_objs, envir = envir),"srcref"),"srcfile")$filename
  if (is.null(filename)) {
    filename <- NA_character_
  }
  filename
}

#' Use 'getParseData' on Object
#'
#' @param one_obj_name one element of obj_name column.
#' @param envir environment where object lives, passed to 'get'.
#'
#' @return data.frame returned by 'getParseData()'.
#' @noRd
get_parse_data <- function(one_obj_name, envir) {
  parse_data <- getParseData(get(one_obj_name, envir = envir), includeText = NA)
  parse_data
}
