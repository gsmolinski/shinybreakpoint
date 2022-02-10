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
    put_browser(object)
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
    restore_body_funs(file, envir)
    object <- utils::findLineNum(file, line, envir = envir, lastenv = envir, nameonly = FALSE)
    if (length(object) > 0) {
      object <- object[[length(object)]]
      list(name = object$name,
           at = object$at,
           envir = object$env)
    } else {
      NULL
    }
}

#' Restore Body of Functions from Chosen File.
#'
#' @param file full path to file where is defined obj to put 'browser()'.
#' @param envir environment where should be object to which user wants to put browser().
#'
#' @return
#' Used for side effect - set 'body()' for funs.
#' @details
#' It is necessary to retrieve original body of fun even if we have deleted added code (see
#' 'put_browser()' function). This is needed to get adequate 'at' from 'findLineNum()' when putting
#' again 'browser()' to the same location. Because we don't know yet which function we are looking for,
#' we need to retrieve body of all functions, but only functions to do not introduce any side effects.
#' @noRd
restore_body_funs <- function(file, envir) {
  original_file <- parse(file)
  original_file_only_fun <- Filter(is_fun, original_file)
  if (length(original_file_only_fun) > 0) {
    e <- new.env()
    for (i in seq_along(original_file_only_fun)) {
      try(eval(original_file_only_fun[[i]], envir = e), silent = TRUE)
    }
    obj_changed <- sort(names(envir)[names(envir) %in% names(e)])
    obj_original <- sort(names(e)[names(e) %in% names(envir)])
    mapply(retrieve_body, obj_changed, obj_original, MoreArgs = list(envir = envir, e = e))
  }
}

#' Set Original Body of Object
#'
#' Helper function for 'restore_body_funs'.
#'
#' @param obj_changed all objects from chosen app environment.
#' @param obj_original all objects from environment in which parsed file was evaluated.
#' @param e environment in which objects from file was evaluated.
#' @param envir environment in which exists objects in app.
#'
#' @return
#' Used for side effect - change body of object.
#' @noRd
retrieve_body <- function(obj_changed, obj_original, envir, e) {
  body(envir[[obj_changed]]) <- body(e[[obj_original]])
}

#' Insert 'browser()' and Code to Remove 'browser()'
#'
#' @param object list with object's name in which 'browser()' will be inserted, indices to know
#' in which location of 'body()' code should be inserted, environment in which object lives and
#' full path to file in which object is defined.
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
put_browser <- function(object) {
  location_in_fun <- object$at[[length(object$at)]] - 1
  at <- object$at[-length(object$at)] # safe, because we are working only on reactives nested in functions
  code <- list(
    substitute(browser()),
    str2lang(construct_obj_with_envir_label(object$envir)),
    substitute(....envirr <- shinybreakpoint:::get_envir(....envirr, rlang::current_env())),
    str2lang(remove_body_expr(object$name, at, location_in_fun)),
    substitute(shiny::getDefaultReactiveDomain()$reload())
  )
  next_line <- seq_along(code) - 1

  mapply(insert_code, code, next_line, MoreArgs = list(envir = object$envir,
                                                       name = object$name,
                                                       at = at,
                                                       location_in_fun = location_in_fun))

  getDefaultReactiveDomain()$reload()
}

#' Insert Code to the Body of Function
#'
#' Helper function used for 'mapply'.
#'
#' @param code code to insert.
#' @param next_line what to add to the location_in_fun to move to the next line?
#' @param envir environment in which should be searching object - object where code will be insert.
#' @param name name of searching object.
#' @param at indices for 'body()' indicating where is searching location.
#' @param location_in_fun starting line in where to put code.
#'
#' @return
#' Used for side effect - insert code to the body of chosen function.
#' @noRd
insert_code <- function(code, next_line, envir, name, at, location_in_fun) {
  body(envir[[name]])[[at]] <- as.call(append(as.list(body(envir[[name]])[[at]]),
                                              code,
                                              location_in_fun + next_line))
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
  expr <- paste0("....envirr <- ", "'", rlang::env_label(envir), "'")
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
#' @param location_in_fun where in object (function) 'browser()' was inserted?
#'
#' @return
#' Character length 1 with correct R syntax which removes all inserted code.
#' @details
#' Returned value will be added to the body of functio, but all variables will be
#' evaluate in 'shinybreakpoint' environment.
#' @noRd
remove_body_expr <- function(name, at, location_in_fun) {
  body_fun <- paste0("body(....envirr[[", "'", name, "'","]])[[c(", paste0(at, collapse = ", "), ")]]")
  added_lines <- c(1, 2, 3, 4, 5)
  what_remove <- paste0("[-c(", paste0(c(location_in_fun + added_lines), collapse = ", "), ")]")
  expr <- paste0(body_fun, " <- ", body_fun, what_remove)
  expr
}
