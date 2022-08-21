#' UI Module
#'
#' Add keydown listener.
#'
#' @param id from 'shinybreakpointServer'. Can be chosen by user.
#'
#' @return HTML script tag with JavaScript code - returns
#' key pressed.
#' @import shiny
#' @noRd
shinybreakpointUI <- function(id) {
  ns <- NS(id)
  key_pressed <- ns("key_pressed")
  js_key <- glue::glue_safe('
    document.addEventListener("keydown", function(e) {{
     Shiny.setInputValue("{key_pressed}", e.key, {{priority: "event"}});
    }});
  ')
  singleton(tags$head(tags$script(HTML(js_key))))
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
#' @param id namespace used for all inputs and outputs in this module.
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
#' objects which would be used directly in the `server`.
#'
#' `shinybreakpoint` module was developed having Bootstrap 5
#' in mind, that's why it is recommended to use [bslib::bs_theme()]
#' to set up Bootstrap 5. Otherwise the UI experience will be
#' worse.
#'
#' Possibility to filter reactive context depending on specific
#' `input` or `output` needs [shiny::reactlog] enabled,
#' which is done by `options(shiny.reactlog = TRUE)`. This line
#' of code needs to be removed before app will be sent to production
#' (which is of course true also for
#' `shinybreakpoint::shinybreakpointServer()`).
#'
#' See example for the idea of how to include all of this
#' requirements in the app. This can also be done by
#' [shinybreakpoint::snippet()] for new apps.
#'
#' @export
#' @import shiny
#' @importFrom magrittr %>%
#' @examples
#' # To run example, copy-paste to file, save
#' # the file and run the app. Then press "F4"
#' # to open the modal dialog.
#'
#' \dontrun{
#'
#' library(shiny)
#'
#' options(shiny.reactlog = TRUE) # TODO: remove
#'
#' appServer <- function(input, output, session) {
#'   observe({
#'     input$num
#'   }, label = "observe_print_num_input")
#' }
#'
#' shinyApp(
#'   ui = fluidPage(
#'     theme = bslib::bs_theme(5),
#'     numericInput("num", "Num", 0)
#'   ),
#'   server = function(input, output, session) {
#'     shinybreakpoint::shinybreakpointServer() # TODO: remove
#'     appServer(input, output, session)
#'   }
#' )
#' }
shinybreakpointServer <- function(keyEvent = "F4",
                                  id = "shinybreakpoint",
                                  varName = "....envirr") {

  check_requirements_shinybreakpointServer(keyEvent, id, varName)
  insertUI("head", "beforeEnd", shinybreakpointUI(id), immediate = TRUE)
  insertUI("head", "beforeEnd", get_element_id(id), immediate = TRUE)
  insertUI("head", "beforeEnd", singleton(shinyjs::useShinyjs()), immediate = TRUE)
  insertUI("head", "beforeEnd", insert_css(), immediate = TRUE)

  caller_envir <- rlang::caller_env()
  filenames_src_code_envirs <- prepare_src_code(caller_envir)
  reactlog_data <- tryCatch(reactlog(), error = function() NULL)
  try(reactlogReset(), silent = TRUE)
  if (length(reactlog_data) > 0 && nrow(filenames_src_code_envirs$filenames_parse_data) > 0) {
    labelled_observers <- filenames_src_code_envirs$labelled_observers
    binded_filenames_parse_data <- prepare_filenames_parse_data(filenames_src_code_envirs$filenames_parse_data)
    dependency_df_ids_data_all_ids <- prepare_dependency_df_and_ids_data(reactlog_data, labelled_observers)
    getDefaultReactiveDomain()$sendCustomMessage("shinybreakpoint_reactlog_ids", dependency_df_ids_data_all_ids$ids_data$label)
  }

  moduleServer(
    id,
    function(input, output, session) {

      find_dependencies_last_input <- reactive({
        req(input$last_input)
        req(length(reactlog_data) > 0)
        validate(check_duplicated_ids(dependency_df_ids_data_all_ids$ids_data))

        stats::setNames(lapply(input$last_input, find_dependencies,
                               binded_filenames_parse_data = binded_filenames_parse_data,
                               reactlog_dependency_df = dependency_df_ids_data_all_ids$reactlog_dependency_df,
                               all_react_ids = dependency_df_ids_data_all_ids$all_react_ids,
                               ids_data = dependency_df_ids_data_all_ids$ids_data), input$last_input)
      })

      find_dependencies_chosen_id <- reactive({
        req(input$chosen_id)
        req(length(reactlog_data) > 0)
        validate(check_duplicated_ids(dependency_df_ids_data_all_ids$ids_data))

        stats::setNames(lapply(input$chosen_id, find_dependencies,
                               binded_filenames_parse_data = binded_filenames_parse_data,
                               reactlog_dependency_df = dependency_df_ids_data_all_ids$reactlog_dependency_df,
                               all_react_ids = dependency_df_ids_data_all_ids$all_react_ids,
                               ids_data = dependency_df_ids_data_all_ids$ids_data), input$chosen_id)
      })

      observe({
        if (length(input$last_input_chosen_id) == 2) {
          shinyWidgets::updateRadioGroupButtons(session, "last_input_chosen_id", selected = NULL)
        }
      }, priority = 3)

      get_app_mode_src_code <- reactive({
        if (isTruthy(input$last_input_chosen_id) && input$last_input_chosen_id == "last_input") {
          list(mode = "last_input",
               data = find_dependencies_last_input())
        } else if (isTruthy(input$last_input_chosen_id) && input$last_input_chosen_id == "chosen_id") {
          list(mode = "chosen_id",
               data = find_dependencies_chosen_id())
        } else {
          list(mode = "files",
               data = stats::setNames(filenames_src_code_envirs$filenames_parse_data$parse_data,
                                      filenames_src_code_envirs$filenames_parse_data$filename_full_path))
        }
      })

      observe({
        req(input$key_pressed == keyEvent)
        showModal(modal_dialog(session, filenames_src_code_envirs$filenames_parse_data, get_app_mode_src_code()))

        req(get_app_mode_src_code()$mode == "files")
        req(nrow(filenames_src_code_envirs$filenames_parse_data) > 0 && !is.null(filenames_src_code_envirs$filenames_parse_data))
        if ((length(filenames_src_code_envirs$filenames_parse_data$filename_full_path) < 9)) {
          shinyWidgets::updateRadioGroupButtons(session, "element",
                                                selected = get_src_editor_file(filenames_src_code_envirs$filenames_parse_data$filename_full_path))
        } else {
          updateSelectizeInput(session, "element",
                               selected = get_src_editor_file(filenames_src_code_envirs$filenames_parse_data$filename_full_path))
        }
      }, priority = 2)

      output$src_code <- reactable::renderReactable({
        req(input$element)
        src_data <- get_app_mode_src_code()$data[[input$element]]
        reactable::reactable(src_data[c("line", "src_code")],
                             columns = list(line = reactable::colDef(align = "center",
                                                                     vAlign = "center",
                                                                     width = 60,
                                                                     name = "",
                                                                     style = list(color = "#8b8589")),
                                            src_code = reactable::colDef(name = "",
                                                                         style = list(whiteSpace = "pre-wrap", color = "#2f4f4f"),
                                                                         cell = reactable::JS(colorize_code())
                                            )),
                             columnGroups = list(reactable::colGroup(name = basename(input$element),
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

      selected_row <- reactive({
        req(input$element)
        shinyjs::removeCssClass(class = "shinybreakpoint-activate-btn-ready",
                                selector = ".shinybreakpoint-modal .shinybreakpoint-activate-btn")
        reactable::getReactableState("src_code", "selected")
      })

      selected_file <- reactive({
        req(selected_row())
        if (get_app_mode_src_code()$mode == "files") {
          input$element
        } else {
          src_code <- get_app_mode_src_code()$data[[input$element]]
          src_code$filename_full_path[selected_row()]
        }
      })

      selected_line <- reactive({
        req(selected_row())
        src_code <- get_app_mode_src_code()$data[[input$element]]
        src_code$line[selected_row()]
      })

      selected_envir <- reactive({
        req(selected_file())
        envir_label <- filenames_src_code_envirs$filenames_parse_data$env_label[[filenames_src_code_envirs$filenames_parse_data$filename_full_path == selected_file()]]
        filenames_src_code_envirs$envirs[[envir_label]]
      })

      object <- reactive({
        req(selected_envir())
        find_object(selected_file(), selected_line(), selected_envir())
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
          shinyjs::addCssClass(class = "shinybreakpoint-activate-btn-ready",
                               selector = ".shinybreakpoint-modal .shinybreakpoint-activate-btn")
        } else {
          shinyjs::addCssClass(class = "shinybreakpoint-not-set",
                               selector = ".shinybreakpoint-modal .rt-tr-selected")
        }
      }, priority = 1)

      observe({
        req(breakpoint_can_be_set())
        exact_line <- determine_line(selected_file(), selected_line(), object()$envir, object()$at)
        put_browser(object(), varName)
        set_attrs(selected_file(), exact_line, object()$name, object()$envir, object()$at, caller_envir)
        getDefaultReactiveDomain()$reload() # trigger the changes in the body of fun
      }) %>%
        bindEvent(input$activate)
    }
  )
}

#' Create Modal Dialog
#'
#' @param session used in 'create_UI'.
#' @param filenames_src_code used in 'create_UI'.
#' @param mode_src_code returned by reactive `get_app_mode_src_code`.
#' The aim is to display source code for files or for IDs.
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
modal_dialog <- function(session, filenames_src_code, mode_src_code) {
  tags$div(class = "shinybreakpoint-modal",
    modalDialog(
      footer = NULL,
      size = "xl",
      easyClose = TRUE,
      create_UI(session, filenames_src_code, mode_src_code),
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
#' @param mode_src_code returned by reactive `get_app_mode_src_code`.
#' The aim is to display source code for files or for IDs.
#'
#' @return
#' UI in modal dialog - only a message if no appropriate file found or
#' button, list of files / IDs and table with source code.
#' @noRd
create_UI <- function(session, filenames_src_code, mode_src_code) {
  if (is.null(filenames_src_code) || nrow(filenames_src_code) == 0) {
    UI <- tags$div(class = "no-file",
                   tags$div(class = "circle-div",
                            tags$div(class = "circle")),
                   tags$p("There is nothing to see here"))
  } else {

    if (mode_src_code$mode == "files") {
      choices <- stats::setNames(names(mode_src_code$data),
                                 basename(names(mode_src_code$data)))
    } else {
      choices <- names(mode_src_code$data)
    }

    if (length(mode_src_code$data) < 9) {
      elements <- shinyWidgets::radioGroupButtons(session$ns("element"), label = "",
                                                  choices = choices,
                                                  direction = "vertical") %>%
        tagAppendAttributes(class = "shinybreakpoint-radioGroupButtons")
    } else {
      elements <- selectizeInput(session$ns("element"), label = "",
                                 choices = choices,
                                 width = "100%") %>%
        tagAppendAttributes(class = "shinybreakpoint-selectInput")
    }

    UI <- tagList(
      fluidRow(
        column(3, class = "col-xl-2",
               fluidRow(
                 column(4,
                        tags$div(class = "shinybreakpoint-div-activate",
                                 actionButton(session$ns("activate"), label = "", icon = icon("circle"), class = "shinybreakpoint-activate-btn"))
                        ),
                 column(4, offset = 4,
                        tags$div(class = "shinybreakpoint-div-last_input_chosen_id",
                                 shinyWidgets::checkboxGroupButtons(session$ns("last_input_chosen_id"),
                                                                    choices = c(`<i class="fa-solid fa-backward"></i>` = "last_input", `<i class="fa-solid fa-hand-pointer"></i>` = "chosen_id")) %>%
                                   tagAppendAttributes(class = "shinybreakpoint-checkboxGroupButtons-last_input_chosen_id"))
                        )
               ),
               HTML(rep("<br/>", 2)),
               tags$div(class = "shinybreakpoint-div-elements",
                        elements
               ),
               tags$div(id = "br-shinybreakpoint-name"),
               tags$div(id = "shinybreakpoint-name-div",
                        tags$p("shinybreakpoint", id = "shinybreakpoint-name")
               )
        ),
        column(9, class = "col-xl-10",
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
