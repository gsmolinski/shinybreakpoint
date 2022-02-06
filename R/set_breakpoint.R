set_breakpoint <- function(file, line, envir, is_top_line) {
  object <- find_object(file, line, envir, is_top_line)
  if (!is.null(object)) {

  }
}

#' Find Object in which to Put 'browser()'
#'
#' @param file full path to file
#' @param line line chosen by user to put browser()
#' @param envir environment where should be object to which user wants to put browser()
#' @param is_top_line does user choose line inside reactive or reactive itself?
#'
#' @return
#' NULL if no objects found (it can happen if object does not live in the default environment) or
#' named list with name of object, indices (for body()) where to put browser() and environment
#' in which object was found.
#' @details
#' If object lives in default environment, then everything will be fine, however if it does not live
#' in default environment, breakpoint would not be set (e.g. it can live in global environment
#' if user explicitly assigned it to the global environment).
#'
#' If top line was chosen, i.e. line from which reactive context starts, then
#' we would add 1 to the line to ensure 'browser()' will be put inside reactive context.
#' @noRd
find_object <- function(file, line, envir, is_top_line) {
  if (is_top_line) {
    line <- line + 1
  }
  object <- utils::findLineNum(file, line, envir = envir, lastenv = envir)
  if (length(object) > 0) {
    object <- object[[length(object)]]
    list(name = object$name,
         at = object$at,
         envir = object$env)
  } else {
    NULL
  }
}
