set_breakpoint <- function(file, line, envir, is_top_line) {
  object <- find_object(file, line, envir)
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
#' NULL if no objects found (it can happen if object do not lives in the default environment) or
#' names list with name of object, indices (for body()) where to put browser() and environment
#' in which object was found.
#' @details
#' If object lives in default environment, then everything will be fine, however if it does not live
#' in default environment, breakpoint would not be set (e.g. it can live in global environment
#' if user explicitly assigned it to the global environment).
#'
#' If top line was chosen, i.e. line from which reactive context starts, then we would put
#' 'observer(browser())' outside the reactive function (but hopefully inside 'server') instead
#' of 'browser()' inside reactive function. Thanks for that it would be possible to browsing
#' 'server' itself, not only specific reactive context inside 'server'.
#' @noRd
find_object <- function(file, line, envir, is_top_line) {
  object <- utils::findLineNum(file, line, envir = envir, lastenv = envir)
  if (length(object) > 0) {
    object <- object[[length(object)]]
    if (is_top_line) {
      object$at <- 0
    }
    list(name = object$name,
         at = object$at,
         envir = object$env)
  } else {
    NULL
  }
}
