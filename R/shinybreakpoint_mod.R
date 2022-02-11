#' UI Module
#'
#' Add keydown listener.
#'
#' @param id from 'shinybreakpointServer'. Can be chosen by user.
#'
#' @return HTML script tag with JavaScript code.
#' @import shiny
shinybreakpointUI <- function(id) {
  ns <- NS(id)
  inputId <- ns("key_pressed")
  js <- glue::glue_safe(
    'document.addEventListener("keydown", function(e) {{
     Shiny.onInputChange({inputId}, e.key);
    }})'
  )
  tags$script(js)
}

shinybreakpointServer <- function(keyEvent = "F1",
                                  id = "shinybreakpoint") {

  check_requirements_shinybreakpointServer(keyEvent, id)

  insertUI("head", "beforeEnd", shinybreakpointUI(id), immediate = TRUE)

  moduleServer(
    id,
    function(input, output, session) {

    }
  )
}
