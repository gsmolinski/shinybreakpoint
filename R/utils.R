check_requirements_shinybrowserServer <- function(enabled, id, keyEvent, session) {

  if (!is.logical(enabled) || is.na(enabled) || length(enabled) > 1) {
    stop("'enabled' must be TRUE or FALSE.",
         call. = FALSE)
  }

  if (!is.character(id) || is.na(id) || length(id) > 1 || id == "") {
    stop("'id' must be of type character of length 1 and can't be NA or empty.",
         call. = FALSE)
  }

  if (!is.character(keyEvent) || is.na(keyEvent) || length(keyEvent) > 1 || keyEvent == "") {
    stop("'keyEvent' must be of type character of length 1 and can't be NA or empty.",
         call. = FALSE)
  }

  if (is.null(session)) {
    stop("No Shiny session object was found.",
         call. = FALSE)
  }
}
