#' Write (Append) Basic Shiny App With Shinybreakpoint Functionality
#'
#' Append code skeleton with `shinybreakpoint` functionality to the
#' active (opened) file in RStudio.
#'
#' @return
#' Appends code skeleton to the file.
#'
#' @details
#' `shinybreakpoint` needs server logic to be included in the function separated
#' from the 'server' part of app and also was developed having Bootstrap
#' version 5 in mind. This snippet takes care of that.
#'
#' Snippet is also available as the addin.
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#' # use shinybreakpoint::snippet() in the Console in RStudio
#' # if there is an opened file (in Source Editor),
#' # then this skeleton should be added to the file:
#' library(shiny)
#'
#' ui <- fluidPage(
#'   theme = bslib::bs_theme(5),
#' )
#'
#' appServer <- function(input, output, session) {
#'
#' }
#'
#' server <- function(input, output, session) {
#'   shinybreakpoint::shinybreakpointServer() # TODO: remove
#'   appServer(input, output, session)
#' }
#'
#' shinyApp(ui, server)
#' }
snippet <- function() {
  id <- tryCatch(rstudioapi::getSourceEditorContext()$id,
                   error = function(e) NULL)
  if (is.null(id)) {
    stop("Can't find opened file in RStudio.", call. = FALSE)
  } else {
    snippet_template <- readLines(file.path(system.file("snippet", package = "shinybreakpoint"),
                                            "snippet_template.R"))
    invisible(rstudioapi::insertText(text = snippet_template, id = id))
  }
}
