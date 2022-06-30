#' Add JS Code To Track Last Changed Input
#'
#' @param id from `shinybreakpointServer`. Can be chosen by user.
#'
#' @return
#' HTML script tag with JavaScript code - returns last changed
#' input which is not from `shinybreakpoint` namespace.
#' @import shiny
#' @noRd
track_last_input <- function(id) {
  ns <- NS(id)
  last_input <- ns("last_input")
  js_track <- glue::glue_safe('
                         document.addEventListener("shiny:inputchanged", function(e) {{
                          if (!e.startsWith("{id}-")) {{
                            Shiny.setInputValue("{last_input}", e.name);
                          }}
                         }});
                        ')
  singleton(tags$head(tags$script(HTML(js_track))))
}
