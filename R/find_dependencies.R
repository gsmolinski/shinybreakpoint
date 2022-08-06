#' Find Dependencies Based On Info From [reactlog]
#'
#' Provides functionality to show the user reactive context (reactives, observers,
#' outputs) based on chosen id - reactive context which depends on some id.
#'
#' @param id name of the input or output id
#' @param filenames_parse_data data.frame with filenames and parse data for each
#' @param labelled_observers data.frame with labelled observers - start line, end line, label, filename or NULL.
#' @param reactlog_data data returned by [reactlog]
#'
#' @return
#' data.frame with columns:
#' - filename
#' - lines
#' - reactive context (reactive, observe or output)
#' which belongs as a dependencies to the `id`.
#' @details
#' User can choose input or output id in the app (the idea is to be able to choose
#' something which is visible) and then, based on this id, this function will find
#' all dependencies, so user can see all relevant parts of code for some input or
#' output.
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @noRd
find_dependencies <- function(id, filenames_parse_data, labelled_observers, reactlog_data) {
  ids_data <- prepare_ids_data(reactlog_data, labelled_observers)

  id_type <- ids_data$type[ids_data$react_id == id]
  ids_data <- ids_data %>%
    filter(!is.na(.data$file))

  reactlog_dependency_df <- dplyr::bind_rows(lapply(reactlog_data, get_dependencies_from_reactlog))
  dependency_graph <- construct_dependency_graph(reactlog_dependency_df, id_type)


}

#' Prepare Data With Ids And Other Information From [reactlog]
#'
#' Join together data.frame with labelled observers and data.frame constructed
#' from [reactlog] to get for each id possible: filename and line where this
#' object (output, observer, reactive) is present as well as type of id
#' (output or input)
#'
#' @param reactlog_data data returned by [reactlog].
#' @param labelled_observers data.frame with labelled observers - start line, end line, label, filename or NULL.
#'
#' @return
#' data.frame with columns:
#' - react_id: react id from [reactlog], i.e. id specific for [reactlog]
#' - label: id used in app (as input or output) or label for observer or name for reactive
#' - filename: basename of file (not a full path!)
#' - location: for reactive or output it is one line in which this reactive or output can be find
#' - type: if the label is an input, output or something else (then NA)
#' - line1: for labelled observers this is started line for this observer
#' - line2: for labelld observers this is end line for this observer
#' Even if no elements in the App, this fun still should return data.frame with 1 row: Theme Counter
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @noRd
prepare_ids_data <- function(reactlog_data, labelled_observers) {
  ids_data <- dplyr::bind_rows(lapply(reactlog_data, extract_ids_data_to_df))
  ids_data <- ids_data %>%
    dplyr::mutate(type = dplyr::case_when(grepl("^input\\$", .data$label, perl = TRUE) ~ "input",
                                          grepl("^output\\$", .data$label, perl = TRUE) ~ "output",
                                          TRUE ~ NA_character_),
                  label = gsub("^input\\$|^output\\$", "", .data$label, perl = TRUE))
  if (!is.null(labelled_observers)) {
    ids_data <- dplyr::left_join(ids_data, labelled_observers, by = "label")
    ids_data <- ids_data %>%
      mutate(filename = ifelse(is.na(.data$filename), .data$file, .data$filename)) %>%
      dplyr::select(-.data$file)
  }

  ids_data
}

#' Get Id And Other Data From List Returned By [reactlog]
#'
#' Helper function for `lapply` used in `prepare_ids_data`.
#'
#' @param reactlog_data data returned by [reactlog].
#'
#' @return
#' data.frame
#' @noRd
extract_ids_data_to_df <- function(reactlog_data) {
  if (reactlog_data$action == "define") {
    data.frame(react_id = reactlog_data$reactId,
               label = reactlog_data$label,
               filename = attr(reactlog_data$label, "srcfile"),
               location = attr(reactlog_data$label, "srcref")[[1]])
  }
}

#' Extract ReactId And ReactId It Depends On And Put It To Data Frame
#'
#' Helper function for `lapply` used in `find_dependencies`.
#'
#' @param reactlog_data data returned by [reactlog].
#'
#' @return
#' data.frame with two columns:
#' - react_id: name of ReactId
#' - depends_on_react_id: ReactId On which ReactId from column 1 depends on
#' @noRd
get_dependencies_from_reactlog <- function(reactlog_data) {
  if (reactlog_data$action == "dependsOn") {
    data.frame(react_id = reactlog_data$reactId,
               depends_on_react_id = reactlog_data$depOnReactId)
  }
}

#' Construct Dependency Graph
#'
#' Find all connected ReactIds and mark them as belong
#' to the same group (graph).
#'
#' @param reactlog_dependency_df data.frame returned by `get_dependencies_from_reactlog`.
#' @param id_type "output" or "input".
#'
#' @return
#' data.frame with two columns:
#' - graph indicates group to which reactIds belong
#' - readId is a name (Id) of reactive from reactlog
#' @details
#' If the id is of type "output", we don't want to find all connected
#' components, because we could find reactive which depends on the same
#' input, but is not connected with specific output at all, e.g. two
#' outputs which depends on the same input$text, but have nothing common
#' more than that. To avoid this, we can just remove inputs from data.frame
#' if the chosen id is of type output - this is safe, because we won't display
#' any parts of code with "inputs" for the user to set breakpoint.
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @noRd
construct_dependency_graph <- function(reactlog_dependency_df, id_type) {
  if (id_type == "output") {
    reactlog_dependency_df <- reactlog_dependency_df %>%
      dplyr::filter(.data$type != "input" | is.na(.data$type))
  }

  graph_as_data_frame <- igraph::graph_from_data_frame(reactlog_dependency_df) %>%
    igraph::components() %>%
    igraph::membership() %>%
    utils::stack()

  names(graph_as_data_frame) <- c("graph", "react_id")
  graph_as_data_frame
}
