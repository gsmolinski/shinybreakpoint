#' Write Basic Shiny App With Shinybreakpoint Functionality
#'
#' Add code skeleton with `shinybreakpoint` functionality to the
#' active (opened) file in RStudio if this file is saved.
#'
#' @param append should the code be added to the existing content or replace it? Added (`TRUE`) by default.
#'
#' @return
#' Writes code skeleton to the file.
#'
#' @details
#' `shinybreakpoint` needs server logic to be included in the function separated
#' from the 'server' part of app and also was developed having latest Bootstrap
#' version in mind. This snippet takes care of that.
#'
#' It may be necessary to click inside the file after this function was used
#' to refresh the editor.
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#' # use shinybreakpoint::snippet() in the Console in RStudio
#' # if there is an opened and saved file (in Source Editor),
#' # then this skeleton should be added to the file:
#' library(shiny)
#'
#' ui <- fluidPage(
#'   theme = bslib::bs_theme(),
#' )
#'
#' appServer <- function(input, output, session) {
#'
#' }
#'
#' server <- function(input, output, session) {
#'   appServer(input, output, session)
#'   shinybreakpoint::shinybreakpointServer() # TODO: remove
#' }
#'
#' shinyApp(ui, server)
#' }
snippet <- function(append = TRUE) {
  path <- tryCatch(rstudioapi::getSourceEditorContext()$path,
                   error = function(e) NULL)
  if (is.null(path) || path == "") { # "" if not yet saved file
    stop("Can't find saved and opened file in RStudio", call. = FALSE)
  } else {
    snippet_template <- readLines(file.path(system.file("snippet", package = "shinybreakpoint"),
                                            "snippet_template.R"))
    write(snippet_template, path, append = append)
  }
}
