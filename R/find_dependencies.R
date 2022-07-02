#' Find Dependencies Based On Info From Reactlog
#'
#' Provides functionality to show the user reactive context (reactives, observers,
#' outputs) based on chosen id - reactive context which depends on some id.
#'
#' @param id name of the input or output id
#' @param filenames_parse_data data.frame with filenames and parse data for each
#' @param labelled_observers data.frame with labelled observers - start line, end line, label, filename
#' @param reactlog_data data returned by [reactlog] - dependencies
#'
#' @return
#' data.frame with columns:
#' - filename
#' - lines
#' - reactive context (reactive, observe or output)
#' which belongs as a dependencies to the `id`.
#' @details
#' User can choose input or output id in the app (the idea is to be able to choose
#' something which is visible) and then, based on this id, this function will find
#' all dependencies, so user can see all relevant parts of code for some input or
#' output.
#' @noRd
find_dependencies <- function(id, filenames_parse_data, labelled_observers, reactlog_data) {

}
