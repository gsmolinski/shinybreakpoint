#' UI Module
#'
#' Add keydown listener.
#'
#' @param id from 'shinybreakpointServer'. Can be chosen by user.
#'
#' @return HTML script tag with JavaScript code - returns
#' key pressed and counter (+1 after any key pressed).
#' @details
#' It is needed to add counter, because otherwise when
#' the same key will be pressed in a row, the second
#' input wouldn't have any effect.
#' @import shiny
shinybreakpointUI <- function(id) {
  ns <- NS(id)
  key_counted <- ns("key_counted")
  key_pressed <- ns("key_pressed")
  js <- glue::glue_safe('
    let counter = 0;
    document.addEventListener("keydown", function(e) {{
     Shiny.onInputChange("{key_counted}", counter++);
     Shiny.onInputChange("{key_pressed}", e.key);
    }})
  ')
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
