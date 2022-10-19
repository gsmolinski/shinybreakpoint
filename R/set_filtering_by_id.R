#' Set Option to Filtering by Id
#'
#' Enable [reactlog] functionality and remove
#' data saved in temporary directory when app stops.
#'
#' @return
#' Used for side effect - enable [reactlog]
#' and set up cleaning in temporary directory.
#' @details
#' This function must be used outside of the `server`
#' part of the app.
#' @export
#' @import shiny
#' @examples
#' \dontrun{
#'
#' library(shiny)
#'
#' shinybreakpoint::set_filtering_by_id() # TODO: remove
#'
#' appServer <- function(input, output, session) {
#'   observe({
#'     input$num
#'   }, label = "observe_print_num_input")
#' }
#'
#' shinyApp(
#'   ui = fluidPage(
#'     theme = bslib::bs_theme(5),
#'     numericInput("num", "Num", 0)
#'   ),
#'   server = function(input, output, session) {
#'     shinybreakpoint::shinybreakpointServer() # TODO: remove
#'     appServer(input, output, session)
#'   }
#' )
#' }
set_filtering_by_id <- function() {
  reactlog::reactlog_enable()

  onStop(function() {
    path <- file.path(tempdir(), "_shinybreakpoint_____reactlog.rds")
    if (file.exists(path)) {
      file.remove(path)
    }
  })
}
