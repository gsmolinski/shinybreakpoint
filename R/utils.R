#' Check Requirements to Run 'shinybreakpointServer'
#'
#' @param keyEvent passed from 'shinybreakpointServer'.
#' @param id passed from 'shinybreakpointServer'.
#'
#' @import shiny
#' @noRd
check_requirements_shinybreakpointServer <- function(keyEvent, id) {

  if (!length(keyEvent) == 1 || !is.character(keyEvent) || is.na(keyEvent) || keyEvent == "") {
    stop("'keyEvent' must be of type character of length 1 and can't be NA or empty character.",
         call. = FALSE)
  }

  if (!length(id) == 1 || !is.character(id) || is.na(id) || id == "") {
    stop("'id' must be of type character of length 1 and can't be NA or empty character.",
         call. = FALSE)
  }

  if (is.null(getDefaultReactiveDomain())) {
    stop("No session object was found. This could mean that 'shinybreakpointServer()' was
         not used in the 'server' part of Shiny app.",
         call. = FALSE)
  }
}

#' Check if Expression is an Named Function
#'
#' @param expr expression returned by 'parse()'.
#'
#' @return logical length 1.
#' @noRd
is_named_fun <- function(expr) {
  expr_3 <- try(expr[[3]], silent = TRUE)
  if (class(expr_3) != "try-error") {
    has_assignment(expr) && is.call(expr_3) && as.character(expr_3[[1]]) == "function"
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
  "^reactive$|^eventReactive$|^observe$|^observeEvent$|^render[A-Z]+"
}
