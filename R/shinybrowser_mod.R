#' UI Module
#'
#' Add keydown listener.
#'
#' @param id from 'shinybrowserServer'. Can be chosen by user.
#'
#' @return HTML script tag with JavaScript code.
#' @import shiny
shinybrowserUI <- function(id) {
  ns <- NS(id)
  inputId <- ns("key_pressed")
  js <- glue::glue_safe(
    'document.addEventListener("keydown", function(e) {{
     Shiny.onInputChange({inputId}, e.key);
    }})'
  )
  tags$script(js)
}

shinybrowserServer <- function(keyEvent = "F1",
                               id = "shinybrowser") {

  check_requirements_shinybrowserServer(keyEvent, id)

  insertUI("head", "beforeEnd", shinybrowserUI(id), immediate = TRUE)

  moduleServer(
    id,
    function(input, output, session) {

    }
  )
}
