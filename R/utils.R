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
  if (class(expr_3) != "try-error") {
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

#' Get Path to css File and Insert It.
#'
#' @return
#' HTML which inserts css file.
#' @import shiny
#' @noRd
insert_css <- function() {
  addResourcePath("shinybreakpoint-resources",
                  system.file("www", package = "shinybreakpoint"))

  singleton(tags$link(rel = "stylesheet", type = "text/css",
                      href = file.path("shinybreakpoint-resources", "css", "shinybreakpoint.css")))
}

#' Add 'span' Tag with Given Classes to the Parts of String
#'
#' It allows to colorize the parts of string according to the classes.
#'
#' @param code character length 1.
#'
#' @return
#' Used for side effect - string will be inserted again into the HTML element, but
#' with the 'dangerouslySetInnerHTML' mode (available in React),
#' so the HTML code won't be escaped and thus colors will be visible.
#' @import shiny
#' @noRd
colorize_code <- function() {
  readLines(file.path(system.file("www", package = "shinybreakpoint"),
                      "js",
                      "colorize_code.js"))
}
