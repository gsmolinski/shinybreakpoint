#' UI module
#'
#' Adds keydown listener.
#'
#' @param id arg from shinybrowserServer. Can be chosen by user.
#'
#' @return JavaScript code which adds event listener
#' @import shiny
shinybrowserUI <- function(id) {
  ns <- NS(id)
  inputId <- ns("key_pressed")
  js <- glue::glue(
    'document.addEventListener("keydown", function(e) {{
     Shiny.onInputChange({inputId}, e.key);
    }})'
  )
  tags$script(js)
}

shinybrowserServer <- function(enabled = TRUE,
                               id = "shinybrowser",
                               keyEvent = "F1",
                               session = getDefaultReactiveDomain()) {

  check_requirements_shinybrowserServer(enabled, id, keyEvent, session)

  if (enabled) {
    insertUI("head", "beforeEnd", shinybrowserUI(id), immediate = TRUE)
    moduleServer(
      id,
      function(input, output, session) {

      }
    )
  }
}
