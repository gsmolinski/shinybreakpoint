#' Get Id of Chosen Element
#'
#' @param id from `shinybreakpointServer`. Can be chosen by user.
#'
#' @return
#' HTML script tag with JavaScript code - returns id of chosen
#' element (input or output).
#' @details
#' User can (by ctrl + mouse move) save Id of chosen
#' input or output and then this Id will be used to find
#' dependencies.
#' To let user know that Id was saved, cursor of type
#' 'progress' is displayed.
#' @import shiny
#' @noRd
get_element_id <- function(id) {
  ns <- NS(id)
  chosen_id <- ns("chosen_id")
  js_chosen_id <- glue::glue_safe('
                                  document.addEventListener("mousemove", function(e) {{
                                   if (e.ctrlKey) {{
                                    if (!e.target.id.startsWith("{id}-")) {{
                                     Shiny.setInputValue("{chosen_id}", e.target.id);
                                     document.body.classList.add("shinybreakpoint-cursor-progress");
                                     setTimeout(function() {{
                                      document.body.classList.remove("shinybreakpoint-cursor-progress");
                                     }}, 200);
                                    }}
                                   }}
                                  }})
                                  ')

  singleton(tags$head(tags$script(HTML(js_chosen_id))))
}
