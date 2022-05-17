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
  original_file <- parse(file, keep.source = TRUE)
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
#' (2) location is inside reactive context - needs to check in any previous 'at' except the last two
#' The reason, why the last one or last one and one before last are removed is based on trying, so
#' it may be not reliable - now it only works, nothing more.
#' @noRd
does_breakpoint_can_be_set <- function(object) {
  is_possible <- FALSE
  if (!is.null(object)) {
    envir <- object$envir
    if (length(object$at) > 2) {
      at_this_whole <- object$at[-length(object$at)]
      expr <- body(envir[[object$name]])
      any_reactive_context <- any(vapply(seq_along(object$at[-c(length(object$at) - 1, length(object$at))]),
                                         function(x) is_reactive_context(expr[[object$at[1:x]]]),
                                         FUN.VALUE = logical(1)))
      if (rlang::is_call(expr[[at_this_whole]], "{") && any_reactive_context) {
        is_possible <- TRUE
      }
    }
  }
  is_possible
}

#' Set Attributes to Modified Function
#'
#' This function ensures that during the debugging the whole script from the
#' file will be visible for the user, however, it will be temporary file, not
#' the original file where the breakpoint was set.
#'
#' @param file original file.
#' @param line final line where breakpoint will be set.
#' @param object_name name returned by 'find_object()'.
#' @param object_envir environment returned by 'find_object()'.
#' @param object_at 'at' element (step in the body) returned by 'find_object()'.
#'
#' @return
#' Used for side effects - set attributes to function in which breakpoint was set,
#' so RStudio can find the file and display it for the user. However, we are not
#' using original file, but temporary file - the purpose is to show user the whole
#' context (script) for convenience.
#' @details
#' When the function is modified, attributes are lost, so it is unknown from which
#' file this function comes from. We are constructing temporary file with this modified
#' function and setting the attributes using this temporary file.
#' @noRd
set_attrs <- function(file, line, object_name, object_envir, object_at) {
  path <- tempfile("DEBUGGING_", fileext = ".R")
  write_file_modified(file, line, object_name, object_envir, object_at, path)
  parsed_modified <- parse(path, keep.source = TRUE)
  parsed_modified_only_fun <- Filter(is_named_fun, parsed_modified)
  if (length(parsed_modified_only_fun) > 0) {
    e <- new.env()
    for (i in seq_along(parsed_modified_only_fun)) {
      try(eval(parsed_modified_only_fun[[i]], envir = e), silent = TRUE)
    }
    attr(body(object_envir[[object_name]]), "srcref") <- attr(body(e[[object_name]]), "srcref")
    attr(body(object_envir[[object_name]]), "srcfile") <- attr(body(e[[object_name]]), "srcfile")
    attr(object_envir[[object_name]],"srcref") <- attr(e[[object_name]], "srcref")
  }
}

#' Write Temporary File With Breakpoint Set
#'
#' Breakpoint is set on-the-fly, but to display the user properly
#' the file with the breakpoint, we need to reconstruct this file.
#'
#' @param file original file.
#' @param line final line where breakpoint will be set.
#' @param object_name name returned by 'find_object()'.
#' @param object_envir environment returned by 'find_object()'.
#' @param object_at 'at' element (step in the body) returned by 'find_object()'.
#' @param path path to constructed (temporary) file.
#'
#' @return
#' Used for side effect - writes the file with the breakpoint to the temporary location.
#' @details
#' To restore the attributes (see 'set_attrs()'), we need to parse the file where will be
#' a function with added code (with breakpoint set), so then we can add these attr to the
#' function we have modified and point it to the temporary file, so IDE will open this file
#' during the debugging and user would see the whole script which should be convenient.
#' @noRd
write_file_modified <- function(file, line, object_name, object_envir, object_at, path) {
  added_lines <- c(0, 1, 2, 3) # 4, because 4 lines of code added to the body
  locations_added_code <- replicate(length(added_lines), object_at, simplify = FALSE)
  for (i in seq_along(locations_added_code)) {
    locations_added_code[[i]][[length(locations_added_code[[i]])]] <- locations_added_code[[i]][[length(locations_added_code[[i]])]] + added_lines[[i]]
  }
  added_code <- lapply(locations_added_code, function(e) deparse(body(object_envir[[object_name]])[[e]]))

  file_orig <- as.list(readLines(file, warn = FALSE))
  line <- line - 1 # because we want to add before chosen line
  file_modified <- unlist(append(file_orig, added_code, line), use.names = FALSE)
  writeLines(file_modified, path)
}

#' Determine Exact Line Where Breakpoint Will Be Inserted
#'
#' The line where the 'browser()' will be inserted can be different then
#' line chosen by user. This function returns the exact line.
#'
#' @param file full path to the file.
#' @param line line chosen by user.
#' @param object_envir environment returned by 'find_object()'.
#' @param object_at  'at' element (step in the body) returned by 'find_object()'.
#'
#' @return
#' Line (numeric length 1) where the code (breakpoint) will be inserted.
#' @details
#' If e.g. function is divided into multiple lines and user is choosing the line
#' inside the function, breakpoint won't be set there, but before the function call. This
#' case shows that the line chosen by user can be different than line where the breakpoint
#' will be set. However, to prepare the file with modified function, we need to know
#' the exact line and this is the aim of this function.
#' The idea is that if returned 'at' is the same for two lines, then the breakpoint
#' is set in the first line - so we need to iterate over previous (in the file) lines
#' to check when 'at' has changed.
#' @noRd
determine_line <- function(file, line, object_envir_orig, object_at_orig) {
  still_lines_to_check <- TRUE
  while (still_lines_to_check) {
    line <- line - 1
    obj <- utils::findLineNum(file, line, nameonly = FALSE, envir = object_envir_orig, lastenv = object_envir_orig)
    if (length(obj) > 0) {
      obj <- obj[[length(obj)]]
      at <- obj$at
      if (!identical(object_at_orig, at)) {
        line <- line + 1 # previous checked line was the line where breakpoint will be set
        still_lines_to_check <- FALSE
      }
    } else {
      still_lines_to_check <- FALSE
    }
  }
  line
}

#' Insert 'browser()' and Code to Remove 'browser()'
#'
#' @param object list with object's name in which 'browser()' will be inserted, indices to know
#' in which location of 'body()' code should be inserted, environment in which object lives and
#' full path to file in which object is defined.
#' @param varName passed from 'shinybreakpointServer'. Name used as variable name for environment.
#' Is used in the user's environment, so need to be chosen carefully - keeping in mind possible clashes.
#'
#' @details
#' The point is not to just insert 'browser()', but also to remove 'browser()' immediately after
#' the user exits from debugging mode. It is necessary, because if not removed, 'browser()' will
#' run each time reactive is run.
#'
#' It is also necessary to 'reload()' a session, because then all code inside 'server' is run again
#' and only then changes which was made on 'body()' can actually work - otherwise inserted or removed
#' code won't work, i.e. even that 'body()' will be changed, the previous state will be call. However,
#' because we are taking the body of modified function to construct the file with this function, see
#' 'set_attr' function, we need to reload the session outside this function, currently it is done in the
#' module.
#'
#' This is also the reason why object passed to 'shinyApp(server = server)' as a 'server' cannot be
#' modified using 'body()'- because this object is never rerun, even when the session is
#' reloaded - it is like that, because only the code inside this object is rerun, not the object itself.
#' @import shiny
#' @noRd
put_browser <- function(object, varName) {
  location <- determine_location(object$at)
  envir <- object$envir
  body(envir[[object$name]])[[location$at]] <- as.call(append(as.list(body(envir[[object$name]])[[location$at]]),
                                              get_code_to_put(envir, object$name, location$at, location$location_in_fun,
                                                              var_name = varName),
                                              location$location_in_fun))
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
#'
#' @return list - each element is an line of code to insert.
#' @importFrom rlang !!
#' @noRd
get_code_to_put <- function(envir, name, at, location_in_fun, var_name) {
  envir_label <- rlang::env_label(envir)
  var_sym <- as.symbol(var_name)
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
