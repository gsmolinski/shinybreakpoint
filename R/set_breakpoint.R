#' Title
#'
#' @param file
#' @param line
#' @param envir
#' @param is_top_line does user choose line inside reactive or reactive itself?
#'
#' @return
#'
#' @details
#' If top line was chosen, i.e. line from which reactive context starts, then
#' we would add 1 to the line to ensure 'browser()' will be put inside reactive context.
#' @noRd
set_breakpoint <- function(file, line, envir, is_top_line) {
  if (is_top_line) { # TODO what if next line is bracket? should we check it?
    line <- line + 1
  }
  object <- find_object(file, line, envir)
  if (!is.null(object)) {
    put_browser(object, line)
  }
}

#' Find Object in which to Put 'browser()'
#'
#' @param file full path to file
#' @param line line chosen by user to put browser()
#' @param envir environment where should be object to which user wants to put browser()
#'
#' @return
#' NULL if no objects found (it can happen if object does not live in the default environment) or
#' named list with name of object, indices (for body()) where to put browser(), environment
#' in which object was found and full path to file. It is possible that 'findLineNum'
#' will return more than one object, so this function leaves only last object, i.e. parent object.
#' @details
#' If object lives in default environment, then everything will be fine, however if it does not live
#' in default environment, breakpoint would not be set (e.g. it can live in global environment
#' if user explicitly assigned it to the global environment).
#' @noRd
find_object <- function(file, line, envir) {
  object <- utils::findLineNum(file, line, envir = envir, lastenv = envir)
  if (length(object) > 0) {
    object <- object[[length(object)]]
    list(name = object$name,
         at = object$at,
         envir = object$env,
         full_path = object$filename)
  } else {
    NULL
  }
}

put_browser <- function(object, line) {
  envir <- object$envir
  at <- object$at[-length(object$at)]
  body(envir[[object$name]])[[at]] <- as.call(append(as.list(body(envir[[object$name]])[[at]]),
                                                     substitute(browser()),
                                                     line))
  body(envir[[object$name]])[[at]] <- as.call(append(as.list(body(envir[[object$name]])[[at]]),
                                                     str2lang(paste0("....infor <- list(full_path = ", "'", object$full_path, "'",
                                                                     ", name = ", "'", object$name, "'",
                                                                     ", line = ", line,
                                                                     ", envir_label = ", "'", rlang::env_label(object$envir), "'",
                                                                     ")")),
                                                     line + 1))
  body(envir[[object$name]])[[at]] <- as.call(append(as.list(body(envir[[object$name]])[[at]]),
                                                     substitute(....envirr <- shinybreakpoint:::get_envir(....infor$full_path, ....infor$line, ....infor$envir_label)),
                                                     line + 2))
  body(envir[[object$name]])[[at]] <- as.call(append(as.list(body(envir[[object$name]])[[at]]),
                                                    str2lang(remove_body_expr(object$name, at, line)),
                                                    line + 3))
  body(envir[[object$name]])[[at]] <- as.call(append(as.list(body(envir[[object$name]])[[at]]),
                                                     substitute(shiny::getDefaultReactiveDomain()$reload()),
                                                     line + 4))
  getDefaultReactiveDomain()$reload()
}

get_envir <- function(full_path, line, envir_label) {
  envirs <- rlang::env_parents(rlang::current_env())
  envirs_labels <- vapply(envirs, rlang::env_label, FUN.VALUE = character(1))
  searched_envir <- envirs[[which(envirs_labels == envir_label)]]
  objs <- utils::findLineNum(srcfile = full_path, line = line,
                             envir = searched_envir, lastenv = searched_envir)
  envir <- objs[[length(objs)]]$env
  envir
}

remove_body_expr <- function(name, at, line) {
  body_fun <- paste0("body(....envirr[[", "'", name, "'","]])[[c(", paste0(at, collapse = ", "), ")]]")
  added_lines <- c(1, 2, 3, 4, 5)
  what_remove <- paste0("[-c(", paste0(c(line + added_lines), collapse = ", "), ")]")
  expr <- paste0(body_fun, " <- ", body_fun, what_remove)
  expr
}
