#' Check Requirements to Run 'shinybrowserServer'
#'
#' @param keyEvent passed from shinybrowserServer.
#' @param id passed from shinybrowserServer.
#'
#' @import shiny
#' @noRd
check_requirements_shinybrowserServer <- function(keyEvent, id) {

  if (!length(keyEvent) == 1 || !is.character(keyEvent) || is.na(keyEvent) || keyEvent == "") {
    stop("'keyEvent' must be of type character of length 1 and can't be NA or empty character.",
         call. = FALSE)
  }

  if (!length(id) == 1 || !is.character(id) || is.na(id) || id == "") {
    stop("'id' must be of type character of length 1 and can't be NA or empty character.",
         call. = FALSE)
  }

  if (is.null(getDefaultReactiveDomain())) {
    stop("No session object was found. This could mean that 'shinybrowserServer()' was
         not used in the 'server' function in Shiny app as it should.",
         call. = FALSE)
  }
}
