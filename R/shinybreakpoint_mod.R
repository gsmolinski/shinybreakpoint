#' UI Module
#'
#' Add keydown listener.
#'
#' @param id from 'shinybreakpointServer'. Can be chosen by user.
#'
#' @return HTML script tag with JavaScript code - returns
#' key pressed.
#' @import shiny
shinybreakpointUI <- function(id) {
  ns <- NS(id)
  key_pressed <- ns("key_pressed")
  js <- glue::glue_safe('
    document.addEventListener("keydown", function(e) {{
     Shiny.setInputValue("{key_pressed}", e.key, {{priority: "event"}});
    }});
  ')
  singleton(tags$head(tags$script(HTML(js))))
}

shinybreakpointServer <- function(keyEvent = "F1",
                                  id = "shinybreakpoint") {

  check_requirements_shinybreakpointServer(keyEvent, id)
  insertUI("head", "beforeEnd", shinybreakpointUI(id), immediate = TRUE)
  filenames_src_code_envirs <- prepare_src_code(rlang::caller_env())

  moduleServer(
    id,
    function(input, output, session) {

      observe({
        req(input$key_pressed == keyEvent)
        showModal(modal_dialog(session, filenames_src_code_envirs$filenames_parse_data))
      }) %>%
        bindEvent(input$key_pressed)

      which_file <- reactive({
        req(input$file)
        which(filenames_src_code_envirs$filenames_parse_data$filename_full_path == input$file)
      })

      output$src_code <- reactable::renderReactable({
        reactable::reactable(filenames_src_code_envirs$filenames_parse_data$parse_data[[which_file()]],
                             selection = "single",
                             onClick = "select")
      })

      selected_line <- reactive({
        src_code <- filenames_src_code_envirs$filenames_parse_data$parse_data[[which_file()]]
        row <- reactable::getReactableState("src_code", "selected")
        src_code$line[row]
      })

      object <- reactive({
        req(selected_line())
        file <- filenames_src_code_envirs$filenames_parse_data$filename_full_path[[which_file()]]
        envir <- filenames_src_code_envirs$envirs[[which_file()]]
        find_object(file, selected_line(), envir)
      })

      breakpoint_can_be_set <- reactive({
        req(object())
        does_breakpoint_can_be_set(object())
      })

      observe({
        req(breakpoint_can_be_set())
        reactable::updateReactable("src_code")
      })

      observe({
        req(breakpoint_can_be_set())
        put_browser(object())
      }) %>%
        bindEvent(input$activate)

    }
  )
}

#' Create Modal Dialog
#'
#' @param session passed from 'moduleServer'.
#' @param filenames_src_code data.frame with full paths to files and basenames
#' as well as envir label and src code (but not used here).
#'
#' @return
#' Modal dialog.
#' @details
#' In Bootstrap 3 class 'modal-xl' is not supported and the default
#' ('medium') size of modal is displayed instead. The added script
#' fixes this - size 'large' will be displayed in Bootstrap 3.
#'
#' This additional script should be removed when Shiny will get
#' as a default Bootstrap 4 or higher version.
#' @import shiny
#' @noRd
modal_dialog <- function(session, filenames_src_code) {
  tags$div(class = "shinybreakpoint-modal",
    modalDialog(
      footer = NULL,
      size = "xl",
      easyClose = TRUE,
      fluidRow(
        column(3,
               actionButton(session$ns("activate"), label = "Activate"),
               HTML(rep("<br/>", 2)),
               selectInput(session$ns("file"), label = "File",
                           choices = stats::setNames(filenames_src_code$filename_full_path,
                                                     filenames_src_code$filename))
        ),
        column(9,
               reactable::reactableOutput(session$ns("src_code"))
        )
      ),
      tags$script(HTML('
      if (jQuery.fn.tooltip.Constructor.VERSION.startsWith("3.")) {{
        if (document.getElementById("shiny-modal").children[0].classList.contains("modal-xl")) {{
          document.getElementById("shiny-modal").children[0].classList.remove("modal-xl");
          document.getElementById("shiny-modal").children[0].classList.add("modal-lg");
        }};
      }};
     '))
    )
  )
}
