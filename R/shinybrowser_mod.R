shinybrowserUI <- function(id) {
  ns <- NS(id)
  tagList(

  )
}

shinybrowserServer <- function(enabled = TRUE,
                               id = "shinybrowser",
                               session = getDefaultReactiveDomain()) {

  check_requirements_shinybrowserServer(enabled, id, session)

  if (enabled) {

    moduleServer(
      id,
      function(input, output, session) {

      }
    )
  }
}
