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
#' in default environment, break point would not be set (e.g. it can live in global environment
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
  original_file_only_fun <- Filter(is_named_fun, original_file)
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

#' Check if Possible to Set Breakpoint
#'
#' @param object list with object's name in which 'browser()' will be inserted, indices to know
#' in which location of 'body()' code should be inserted, environment in which object lives and
#' full path to file in which object is defined.
#'
#' @return logical length 1.
#' @details
#' Check if breakpoint can be set before inserting 'browser()' to let user know if this is possible.
#' Breakpoint can be set if:
#' (1) location is inside braces - needs to remove last 'at' to check this and
#' (2) location is inside reactive context - needs to remove two lasts 'at' to check this
#' @noRd
does_breakpoint_can_be_set <- function(object) {
  is_possible <- FALSE
  if (!is.null(object)) {
    envir <- object$envir
    if (length(object$at) > 2) {
      at_this_whole <- object$at[-length(object$at)]
      at_previous_whole <- object$at[-c(length(object$at) - 1, length(object$at))]
      expr <- body(envir[[object$name]])
      if (rlang::is_call(expr[[at_this_whole]], "{") && is_reactive_context(expr[[at_previous_whole]])) {
        is_possible <- TRUE
      }
    }
  }
  is_possible
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
  location <- determine_location(object$at)
  envir <- object$envir
  body(envir[[object$name]])[[location$at]] <- as.call(append(as.list(body(envir[[object$name]])[[location$at]]),
                                              get_code_to_put(envir, object$name, location$at, location$location_in_fun,
                                                              var_name = "....envirr",
                                                              envir_label = rlang::env_label(envir)),
                                              location$location_in_fun))
  getDefaultReactiveDomain()$reload()
}

#' Determine Location in Fun and 'at' Step in Body
#'
#' @param at returned by utils::findLineNum, element 'at'.
#'
#' @return list with (1) location where in body put 'browser' - it is
#' the same as line. (2) at - meaning: step in body, but previous step (i.e.
#' object with curly braces).
#' @noRd
determine_location <- function(at) {
  location_in_fun <- at[[length(at)]] - 1 # append code before chosen line
  if (location_in_fun < 1) {
    location_in_fun <- 1
  }
  at <- at[-length(at)] # safe, because we are working only on reactives nested in functions
  list(location_in_fun = location_in_fun,
       at = at)
}

#' Return Code to Put
#'
#' @param envir returned from utils::findLineNum.
#' @param name returned from utils::findLineNum.
#' @param at returned from determine_location.
#' @param location_in_fun returned from determine_location.
#' @param var_name variable name used in user environment in reactive context.
#' @param var_sym var_name as a symbol.
#'
#' @return list - each element is an line of code to insert.
#' @importFrom rlang !!
#' @noRd
get_code_to_put <- function(envir, name, at, location_in_fun, var_name,
                            envir_label, var_sym = as.symbol(var_name)) {
  list(
    quote(browser()),
    rlang::expr(assign(!!var_name, shinybreakpoint:::get_envir(!!envir_label, rlang::current_env()))),
    remove_body_expr(name, at, location_in_fun, var_sym),
    quote(try(shiny::getDefaultReactiveDomain()$reload(), TRUE))
  )
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
  envirs <- append(current_env, envirs)
  envirs_labels <- vapply(envirs, rlang::env_label, FUN.VALUE = character(1))
  searched_envir <- envirs[[which(envirs_labels == envir_label)]]
  searched_envir
}

#' Construct Expression to Remove Inserted Code
#'
#' @param name name of function to which 'browser()' was inserted.
#' @param at where in body of function 'browser()' was inserted?
#' @param location_in_fun where in object (function) 'browser()' was inserted?
#' @param var_sym used in user environment to store environment where is object
#' in which was put 'browser()'.
#'
#' @return
#' Expression with correct R syntax which removes all inserted code.
#' @details
#' Returned value will be added to the body of function, but all variables will be
#' evaluate in 'shinybreakpoint' environment.
#' @noRd
remove_body_expr <- function(name, at, location_in_fun, var_sym) {
  lines_to_remove <- location_in_fun + c(1, 2, 3, 4) # depends on number of added lines of code
  body_fun <- call("[[", call("body", call("[[", var_sym, name)), at)
  expr <- call("<-", body_fun, call("[", body_fun, call("-", lines_to_remove)))
  expr
}
