#' Get Id of Chosen Element And Its Parents And
#' Track Last Changed Input
#'
#' @param id from `shinybreakpointServer`. Can be chosen by user.
#'
#' @return
#' HTML script tag with JavaScript code - returns id of elements:
#'
#' - chosen input or output: can return multiple ids, because
#' id of current element is taken as well as parent elements.
#' Id is returned only if can be find in reactlog and is not
#' from 'shinybreakpoint' namespace. Also, for safe, duplicated
#' ids are removed. There should be no duplicates in a properly
#' developed Shiny app, but just in case potential duplicates are
#' removed.
#'
#' - last changed input: single value returned and only
#' if can be find in reactlog and is not from 'shinybreakpoint'
#' namespace.
#'
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
  last_input <- ns("last_input")
  chosen_id <- ns("chosen_id")
  js_id <- glue::glue_safe('
                                   $(function() {{

                                    let ids_shiny = [];
                                    Shiny.addCustomMessageHandler("reactlog_ids", function(ids_from_r) {{
                                     if (Array.isArray(ids_from_r)) {{
                                      for (id_one of ids_from_r) {{
                                      if (id_one != null) {{
                                       ids_shiny.push(id_one);
                                      }};
                                     }};
                                     }} else {{
                                      if (ids_from_r != null) {{
                                       ids_shiny.push(ids_from_r);
                                      }};
                                     }};
                                    }});

                                    document.addEventListener("shiny:inputchanged", function(e) {{
                                     if (!e.name.startsWith("{id}-") && ids_shiny.includes(e.name)) {{
                                      Shiny.setInputValue("{last_input}", e.name);
                                     }};
                                    }});

                                    document.addEventListener("mousemove", function(e) {{
                                     if (e.ctrlKey) {{
                                      let ids_all = [];
                                      let ids_correct = [];
                                      let current = e.target;
                                      ids_all.push(current.id);
                                      while (current.parentNode) {{
                                       ids_all.push(current.parentNode.id);
                                       current = current.parentNode;
                                      }};
                                      for (const id_one of ids_all) {{
                                       if (id_one != null && id_one !== "" && !id_one.startsWith("{id}-") && ids_shiny.includes(id_one)) {{
                                        ids_correct.push(id_one);
                                       }};
                                      }};
                                      if (ids_correct.length > 0) {{
                                       let unique_ids_correct = [...new Set(ids_correct)];
                                       Shiny.setInputValue("{chosen_id}", unique_ids_correct);
                                       document.body.classList.add("shinybreakpoint-cursor-progress");
                                       setTimeout(function() {{
                                        document.body.classList.remove("shinybreakpoint-cursor-progress");
                                       }}, 200);
                                      }};
                                     }};
                                    }});

                                   }};
                                  ')

  singleton(tags$head(tags$script(HTML(js_id))))
}
