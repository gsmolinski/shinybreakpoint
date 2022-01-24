#' Colect Filenames, Parse Data and Environments Labels
#'
#' It collects results from 'get_filenames_parse_data' function.
#'
#' @param caller_env object returned by 'rlang::caller_env()', passed by exported function, i.e.
#' function used directly by user.
#'
#' @return named list with:
#' (1) 'filenames_parse_data': data.frame with cols:
#' filename_full_path, filename, parse_data, env_label
#' (2) 'envirs': named list (using rlang::env_label()) with environments
#' or NULL if no objects with srcfile found.
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @noRd
collect_filenames_parse_data <- function(caller_env) {
  envirs <- rlang::env_parents(caller_env)
  envirs <- drop_envs_too_far(envirs)
  filenames_parse_data <- lapply(envirs, get_filenames_parse_data)
  if (length(filenames_parse_data) > 0) {
    filenames_parse_data <- dplyr::bind_rows(filenames_parse_data)

    names(envirs) <- unlist(lapply(envirs, rlang::env_label), use.names = FALSE)
    envirs <- envirs[unique(filenames_parse_data$env_label)]

    filenames_parse_data <- filenames_parse_data %>%
      dplyr::filter(!duplicated(.data$filename_full_path)) %>%
      dplyr::mutate(filename = basename(.data$filename_full_path)) %>%
      dplyr::relocate(.data$filename, .before = .data$parse_data)

    list(filenames_parse_data = filenames_parse_data,
         envirs = envirs)
  }
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
  if (any(grepl("^namespace:|^package:", names(envirs)))) {
    first_namespace <- grep("^namespace:|^package:", names(envirs))[[1]]
    envirs <- envirs[1:first_namespace]
  } else {
    envirs[["global"]] <- NULL
  }
  envirs
}

#' Get Full Path to File, Parse Data for Object and Environment Label
#'
#' @param envir each environment returned by 'rlang::env_parents()',
#' passed by 'collect_filenames_parse_data' function.
#'
#' @return data.frame with cols: filename_full_path, parse_data, env_label or NULL
#' if no objects with srcfile found.
#' @importFrom magrittr %>%
#' @importFrom rlang .data
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
      dplyr::filter(!is.na(.data$filename_full_path) & !duplicated(.data$filename_full_path)) %>%
      dplyr::mutate(parse_data = lapply(.data$obj_name, get_parse_data, envir = envir),
                    env_label = rlang::env_label(envir)) %>%
      dplyr::select(-.data$obj_name)

    filenames_parse_data
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

#' Use 'utils::getParseData' on Object
#'
#' @param one_obj_name one element of obj_name column.
#' @param envir environment where object lives, passed to 'get'.
#'
#' @return data.frame returned by 'getParseData()'.
#' @noRd
get_parse_data <- function(one_obj_name, envir) {
  parse_data <- utils::getParseData(get(one_obj_name, envir = envir), includeText = NA)
  parse_data
}
