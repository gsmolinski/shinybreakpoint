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
         not used in the function which is then used in the 'server' function in Shiny app as it should.",
         call. = FALSE)
  }
}

#' Check if Expression is an Function
#'
#' @param expr expression returned by 'parse()'.
#'
#' @return logical length 1.
#' @noRd
is_fun <- function(expr) {
  obj <- expr[[3]]
  has_assignment(expr) && is.call(obj) && as.character(obj[[1]]) == "function"
}

#' Check if Expression Has Assignment
#'
#' @param expr expression returned by 'parse()'.
#'
#' @return logical length 1.
#' @noRd
has_assignment <- function(expr) {
  is.call(expr) && as.character(expr[[1]]) %in% c("<-", "=", "assign")
}
