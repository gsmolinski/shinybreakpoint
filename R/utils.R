check_requirements_shinybrowserServer <- function(enabled, id, keyEvent) {

  if (length(enabled) > 1 || !is.logical(enabled) || is.na(enabled)) {
    stop("'enabled' must be TRUE or FALSE.",
         call. = FALSE)
  }

  if (length(id) > 1 || !is.character(id) || is.na(id) || id == "") {
    stop("'id' must be of type character of length 1 and can't be NA or empty string.",
         call. = FALSE)
  }

  if (length(keyEvent) > 1 || !is.character(keyEvent) || is.na(keyEvent) || keyEvent == "") {
    stop("'keyEvent' must be of type character of length 1 and can't be NA or empty string.",
         call. = FALSE)
  }
}
