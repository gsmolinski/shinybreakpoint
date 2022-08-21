#' Check Requirements to Run 'shinybreakpointServer'
#'
#' @param keyEvent passed from 'shinybreakpointServer'.
#' @param id passed from 'shinybreakpointServer'.
#' @param varName passed from 'shinybreakpointServer'.
#'
#' @import shiny
#' @noRd
check_requirements_shinybreakpointServer <- function(keyEvent, id, varName) {

  if (!length(keyEvent) == 1 || !is.character(keyEvent) || is.na(keyEvent) || keyEvent == "") {
    stop("'keyEvent' must be of type character of length 1 and can't be NA or empty character.",
         call. = FALSE)
  }

  if (!length(id) == 1 || !is.character(id) || is.na(id) || id == "") {
    stop("'id' must be of type character of length 1 and can't be NA or empty character.",
         call. = FALSE)
  }

  if (!length(varName) == 1 || !is.character(varName) || is.na(varName) || varName == "") {
    stop("'varName' must be of type character of length 1 and can't be NA or empty character.",
         call. = FALSE)
  }

  if (is.null(getDefaultReactiveDomain())) {
    stop("No session object was found. This could mean that 'shinybreakpointServer()' was not used in the 'server' part of Shiny app as it should.",
         call. = FALSE)
  }
}

#' Check if Expression is a Named Function
#'
#' @param expr expression returned by 'parse()'.
#'
#' @return logical length 1.
#' @details
#' 'shiny::*' expr after coercing to character returns character length 3: double colon, package name
#' and function name. Because we need only function name, it is neccesary to use 'rev' - it is safe
#' if used for character length 1.
#' @noRd
is_named_fun <- function(expr) {
  expr_3 <- try(expr[[3]], silent = TRUE)
  if (!inherits(expr_3, "try-error")) {
    has_assignment(expr) && is.call(expr_3) && rev(as.character(expr_3[[1]]))[[1]] == "function"
  } else {
    FALSE
  }
}

#' Check if Expression Has Assignment
#'
#' @param expr expression returned by 'parse()'.
#'
#' @return logical length 1.
#' @noRd
has_assignment <- function(expr) {
  is.call(expr) && as.character(expr[[1]]) %in% c("<-", "<<-", "=", "assign")
}

#' Check if Expression is Reactive Context
#'
#' @param expr expression returned by 'body()'.
#'
#' @return logical length 1.
#' @details
#' 'shiny::*' expr after coercing to character returns character length 3: double colon, package name
#' and function name. Because we need only function name, it is neccesary to use 'rev' - it is safe
#' if used for character length 1.
#' @noRd
is_reactive_context <- function(expr) {
  is.call(expr) && grepl(get_reactive_context_regex(),
                         rev(as.character(expr[[1]]))[[1]], perl = TRUE)
}

#' Get Regular Expression to Indicate Reactive Context
#'
#' @return character length 1.
#' @noRd
get_reactive_context_regex <- function() {
  "^reactive$|^eventReactive$|^observe$|^observeEvent$|^render"
}

#' Get Regular Expression to Indicate Observers
#'
#' @return character length 1.
#' @details
#' Needed to find dependencies based on input or output Id.
#' In reactlog there is no source reference for observers, so
#' we need to find them based on parse data and labels.
#' @noRd
get_observers_regex <- function() {
  "^observe$|^observeEvent$"
}

#' Check If Any Ids Are Duplicated In Data Retrieved From [reactlog]
#'
#' Used in shiny::validate() as a custom error message.
#'
#' @param ids_data collection of ids and other data from [reactlog], returned by `prepare_dependency_df_and_ids_data`
#'
#' @return
#' NULL if no duplicated ids or string if at least
#' one id (label!) is duplicated as well as list with duplicated ids (labels!).
#' @details
#' We need to be sure that Ids (i.e. labels from [reactlog]) are unique, because later we would have
#' problems with retrieving dependencies for chosen Id. Problem is that although in Shiny Ids should
#' be unique, we have rather labels than ids, i.e. labels from observers as well as names of reactives
#' are also in the same vector as Ids. That's why it is quite probably that Ids (labels) may be duplicated
#' if App wasn't developed keeping in mind that Ids and reactives and observers needs to have unique names.
#' @noRd
check_duplicated_ids <- function(ids_data) {
  if (any(duplicated(ids_data$label))) {
    paste0("These Ids or labels are duplicated: ",
                paste0(unique(ids_data$label[duplicated(ids_data$label)]), collapse = ", "),
                ". Use only unique Ids and labels.")
  } else {
    NULL
  }
}

#' Insert CSS dependency.
#'
#' @return
#' htmltools::htmlDependency object with CSS.
#' @noRd
insert_css <- function() {
  htmltools::htmlDependency("shinybreakpoint-css",
                            version = utils::packageVersion("shinybreakpoint"),
                            package = "shinybreakpoint",
                            src = "www",
                            stylesheet = "css/shinybreakpoint.css")
}

#' Add 'span' Tag with Given Classes to the Parts of String
#'
#' It allows to colorize the parts of string according to the classes.
#'
#' @param code character length 1.
#'
#' @return
#' Used for side effect - string will be inserted again into the HTML element, but
#' with the `dangerouslySetInnerHTML` mode (available in React),
#' so the HTML code won't be escaped and thus colors will be visible.
#' @import shiny
#' @noRd
colorize_code <- function() {
  readLines(file.path(system.file("www", package = "shinybreakpoint"),
                      "js",
                      "colorize_code.js"))
}

#' Suppress Note If Package Namespace Is Not Used
#'
#' @return
#' This function should not be used.
#' @details
#' Although `shinybreakpoint` works without
#' packages `reactlog` and `bslib`, these
#' packages should be installed when `shinybreakpoint`
#' is installed, because snippet adds functions from
#' these packages to the appended code.
#' @noRd
add_package_to_import_without_notes <- function() {
  invisible(formals(reactlog::reactlog_disable))
  invisible(formals(bslib::bootstrap))
}
