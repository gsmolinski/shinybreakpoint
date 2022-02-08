#' Set Breakpoint in Chosen Location
#'
#' @param file full path to file in which breakpoint will be set.
#' @param line where to set breakpoint?
#' @param envir environment where lives object in which breakpoint will be set.
#' @param is_top_line does user choose line inside reactive or reactive itself?
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
#' @param file full path to file.
#' @param line line chosen by user to put browser().
#' @param envir environment where should be object to which user wants to put browser().
#'
#' @return
#' NULL if no objects found (it can happen if object does not live in the default environment) or
#' named list with name of object, indices (for body()) where to put browser() and environment
#' in which object was found. It is possible that 'findLineNum'
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
         envir = object$env)
  } else {
    NULL
  }
}

#' Insert 'browser()' and Code to Remove 'browser()'
#'
#' @param object list with object's name in which 'browser()' will be inserted, indices to know
#' in which location of 'body()' code should be inserted, environment in which object lives and
#' full path to file in which object is defined.
#' @param line where to insert 'browser()'?
#'
#' @details
#' The point is not to just insert 'browser()', but also to remove 'browser()' immediately after
#' the user exits from debugging mode. It is necessary, because if not removed, 'browser()' will
#' run each time reactive is run.
#'
#' It is also necessary to 'reload()' a session, because then all code inside 'server' is run again
#' and only then changes which was made on 'body()' can actually work - otherwise inserted or removed
#' code won't work, i.e. even that 'body()' will be changed, the previous state will be call.
#'
#' This is also the reason why object passed to 'shinyApp(server = server)' as a 'server' cannot be
#' modified using 'body()'- because this object is never rerun, even when the session is
#' reloaded - it is like that, because only the code inside this object is rerun, not the object itself.
#' @import shiny
#' @noRd
put_browser <- function(object, line) {
  envir <- object$envir
  at <- object$at[-length(object$at)]
  body(envir[[object$name]])[[at]] <- as.call(append(as.list(body(envir[[object$name]])[[at]]),
                                                     substitute(browser()),
                                                     line))
  body(envir[[object$name]])[[at]] <- as.call(append(as.list(body(envir[[object$name]])[[at]]),
                                                     str2lang(construct_obj_with_envir_label(object$envir)),
                                                     line + 1))
  body(envir[[object$name]])[[at]] <- as.call(append(as.list(body(envir[[object$name]])[[at]]),
                                                     substitute(....envirr <- shinybreakpoint:::get_envir(....envirr_label, rlang::current_env())),
                                                     line + 2))
  body(envir[[object$name]])[[at]] <- as.call(append(as.list(body(envir[[object$name]])[[at]]),
                                                    str2lang(remove_body_expr(object$name, at, line)),
                                                    line + 3))
  body(envir[[object$name]])[[at]] <- as.call(append(as.list(body(envir[[object$name]])[[at]]),
                                                     substitute(shiny::getDefaultReactiveDomain()$reload()),
                                                     line + 4))
  getDefaultReactiveDomain()$reload()
}

#' Construct Expression to Define Object with Environment Label of Chosen Environment
#'
#' It constructs expression using variable which is evaluate in 'shinybreakpoint' package,
#' not in the body of user function.
#'
#' @param envir environment in which object to insert 'browser()' lives.
#'
#' @return
#' Character length 1 with correct R syntax which creates object '....envirr_label' with
#' environment label. It is not possible to coerce environment itself into character type,
#' so label has to be use.
#'
#' @details
#' Returned value will be added to body of function after inserted 'browser()',
#' so label environment can be used in user's function environment.
#' @noRd
construct_obj_with_envir_label <- function(envir) {
  expr <- paste0("....envirr_label <- ", "'", rlang::env_label(envir), "'")
  expr
}

#' Find Environment Using Label and Get It
#'
#' Search across parent environments, starting from current environment to find
#' and retrieve environment of chosen label.
#'
#' @param envir_label label of environment in which lives object to which breakpoint
#' will be inserted. Retrieved using 'rlang::env_label()'.
#' @param current_env 'rlang::current_env()' will be passed as argument to this parameter
#' to ensure that not 'shinybreakpoint' environment, but user environment will be used.
#'
#' @return
#' Environment having the same label as searched.
#' @details
#' To get environment, function has to be run in user's environment, but to find
#' searched environment, we need to know what to search and for this label is used.
#' Environment is necessary to be sure that proper object will be found.
get_envir <- function(envir_label, current_env) {
  envirs <- rlang::env_parents(current_env)
  envirs_labels <- vapply(envirs, rlang::env_label, FUN.VALUE = character(1))
  searched_envir <- envirs[[which(envirs_labels == envir_label)]]
  searched_envir
}

#' Construct Expression to Remove Inserted Code
#'
#' @param name name of function to which 'browser()' was inserted.
#' @param at where in body of function 'browser()' was inserted?
#' @param line where in file 'brpwser()' was inserted?
#'
#' @return
#' Character length 1 with correct R syntax which removes all inserted code.
#' @details
#' Returned value will be added to the body of functio, but all variables will be
#' evaluate in 'shinybreakpoint' environment.
#' @noRd
remove_body_expr <- function(name, at, line) {
  body_fun <- paste0("body(....envirr[[", "'", name, "'","]])[[c(", paste0(at, collapse = ", "), ")]]")
  added_lines <- c(1, 2, 3, 4, 5)
  what_remove <- paste0("[-c(", paste0(c(line + added_lines), collapse = ", "), ")]")
  expr <- paste0(body_fun, " <- ", body_fun, what_remove)
  expr
}
