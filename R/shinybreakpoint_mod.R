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

#' Use Module to Set Breakpoint
#'
#' Module of Shiny app needed to enable functionality to
#' setting breakpoint. Must be used in the `server` part of
#' Shiny app or in the function which is then used in the
#' `server` part of app.
#'
#' @param keyEvent key to run modal dialog with the functionality
#' to set breakpoint. `"F4"` by default.
#' @param id namespace used for all inputs in this module.
#' `"shinybreakpoint"` by default. Change if in the app some
#' other module is used which already has `"shinybreakpoint"`
#' namespace.
#' @param varName setting breakpoint is not equal to inserting only
#' `browser()`, but also additional code, one of which is assignment
#' operation. This parameter determines the variable name to assign
#' value. `"....envirr"` by default. Change if this name is already
#' in use somewhere in the app.
#'
#' @return
#' Used for side effect - adds modal dialog to the Shiny app
#' with the options to set breakpoint. Modal dialog is shown
#' when the key specified in `keyEvent` is pressed.
#' @details
#' One of the core concepts which founds this module is the
#' necessity to re-run the objects present in the `server`
#' part of app. This is possible only when these objects
#' do not live directly in the `server`, but in the function
#' which is then used in `server`. This naturally harmonizes
#' with the modules, but it needs the separate function for
#' objects which would be used directly in the `server`
#' (see example).
#'
#' @export
#' @import shiny
#' @importFrom magrittr %>%
#' @examples
#' # To run example, copy-paste to file, save the file
#' # and run the app. Then press "F4" to open the modal dialog.
#'
#' \dontrun{
#' library(shiny)
#'
#' appServer <- function(input, output, session) {
#'   observe({
#'     input$num
#'   })
#' }
#'
#' shinyApp(
#'   ui = fluidPage(
#'     numericInput("num", "Num", 0)
#'   ),
#'   server = function(input, output, session) {
#'     shinybreakpoint::shinybreakpointServer()
#'     appServer(input, output, session)
#'   }
#' )
#' }
shinybreakpointServer <- function(keyEvent = "F4",
                                  id = "shinybreakpoint",
                                  varName = "....envirr") {

  check_requirements_shinybreakpointServer(keyEvent, id, varName)
  insertUI("head", "beforeEnd", shinybreakpointUI(id), immediate = TRUE)
  insertUI("head", "beforeEnd", singleton(shinyjs::useShinyjs()), immediate = TRUE)
  insertUI("head", "beforeEnd", insert_css(), immediate = TRUE)
  filenames_src_code_envirs <- prepare_src_code(rlang::caller_env())

  moduleServer(
    id,
    function(input, output, session) {

      observe({
        req(input$key_pressed == keyEvent)
        showModal(modal_dialog(session, filenames_src_code_envirs$filenames_parse_data))
        if ((length(filenames_src_code_envirs$filenames_parse_data$filename_full_path) < 9)) {
          shinyWidgets::updateRadioGroupButtons(session, "file",
                                                selected = get_src_editor_file(filenames_src_code_envirs$filenames_parse_data$filename_full_path))
        } else {
          updateSelectizeInput(session, "file",
                               selected = get_src_editor_file(filenames_src_code_envirs$filenames_parse_data$filename_full_path))
        }
      }) %>%
        bindEvent(input$key_pressed)

      which_file <- reactive({
        req(input$file)
        which(filenames_src_code_envirs$filenames_parse_data$filename_full_path == input$file)
      })

      output$src_code <- reactable::renderReactable({
        req(which_file())
        src_data <- filenames_src_code_envirs$filenames_parse_data$parse_data[[which_file()]]
        reactable::reactable(src_data,
                             columns = list(line = reactable::colDef(align = "center",
                                                                     width = 60,
                                                                     name = ""),
                                            src_code = reactable::colDef(name = "",
                                                                         style = list(whiteSpace = "pre-wrap"),
                                            )),
                             columnGroups = list(reactable::colGroup(name = filenames_src_code_envirs$filenames_parse_data$filename[[which_file()]],
                                                                     columns = c("line", "src_code"))),
                             rowClass = function(index) if (is.na(src_data[index, "src_code"])) "shinybreakpoint-na-row",
                             selection = "single",
                             onClick = "select",
                             sortable = FALSE,
                             pagination = FALSE,
                             compact = TRUE,
                             borderless = TRUE,
                             highlight = TRUE,
                             height = "84vh",
                             theme = reactable::reactableTheme(
                               backgroundColor = "#f2eeeb", highlightColor = "#DFD6D2",
                               rowSelectedStyle = list(backgroundColor = "#DFD6D2", boxShadow = "inset 0 3px 5px rgba(0,0,0,.125), 0 3px 5px rgba(0,0,0,.125);")
                             ))
      })

      selected_line <- reactive({
        req(which_file())
        src_code <- filenames_src_code_envirs$filenames_parse_data$parse_data[[which_file()]]
        row <- reactable::getReactableState("src_code", "selected")
        # if row is NULL, then returns numeric(0), which is not truthy
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
        req(object())
        if (isTruthy(breakpoint_can_be_set())) {
          shinyjs::addCssClass(class = "shinybreakpoint-set",
                               selector = ".shinybreakpoint-modal .rt-tr-selected")
        } else {
          shinyjs::addCssClass(class = "shinybreakpoint-not-set",
                               selector = ".shinybreakpoint-modal .rt-tr-selected")
        }
      })

      observe({
        req(breakpoint_can_be_set())
        put_browser(object(), varName)
      }) %>%
        bindEvent(input$activate)

    }
  )
}

#' Create Modal Dialog
#'
#' @param session used in 'create_UI'.
#' @param filenames_src_code used in 'create_UI'.
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
      create_UI(session, filenames_src_code),
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

#' Create UI for Modal Dialog.
#'
#' @param session passed from 'moduleServer'.
#' @param filenames_src_code data.frame with full paths to files and basenames
#' as well as envir label and src code (but not used here).
#'
#' @return
#' UI in modal dialog - only a message if no apriopriate file found or
#' button, list of files and table with source code.
#' @noRd
create_UI <- function(session, filenames_src_code) {
  if (is.null(filenames_src_code)) {
    UI <- tags$div(class = "no-file", tags$p(""))
  } else {
    if (length(filenames_src_code$filename) < 9) {
      files <- shinyWidgets::radioGroupButtons(session$ns("file"), label = "",
                                               choices = stats::setNames(filenames_src_code$filename_full_path,
                                                                         filenames_src_code$filename),
                                               direction = "vertical") %>%
        tagAppendAttributes(class = "shinybreakpoint-radioGroupButtons")
    } else {
      files <- selectizeInput(session$ns("file"), label = "",
                              choices = stats::setNames(filenames_src_code$filename_full_path,
                                                        filenames_src_code$filename), width = "100%") %>%
        tagAppendAttributes(class = "shinybreakpoint-selectInput")
    }

    UI <- tagList(
      fluidRow(
        column(3,
               tags$div(class = "shinybreakpoint-div-activate",
                        actionButton(session$ns("activate"), label = "Activate", class = "shinybreakpoint-activate-btn")
               ),
               HTML(rep("<br/>", 2)),
               tags$div(class = "shinybreakpoint-div-files",
                        files
               ),
               br(),
               tags$div(tags$p("shinybreakpoint", id = "shinybreakpoint-name"))
        ),
        column(9,
               reactable::reactableOutput(session$ns("src_code"))
        )
      )
    )
  }
  UI
}

#' Get Full Path to File Opened in Source Editor in RStudio
#'
#' @param filename_full_path all filename (full paths)
#' returned by 'prepare_src_code()'.
#'
#' @return
#' Filename (full path) opened in RStudio (in Source Editor). If
#' RStudio is not in use (or no file opened) or if opened file is not in the column
#' in object returned by 'prepare_src_code()', then returns the
#' first element of the passed vector.
#' @noRd
get_src_editor_file <- function(filename_full_path) {
  selected <- tryCatch(rstudioapi::getSourceEditorContext()$path,
                       error = function(e) filename_full_path[[1]])
  if (is.null(selected) || !selected %in% filename_full_path) {
    selected <- filename_full_path[[1]]
  }
  selected
}
