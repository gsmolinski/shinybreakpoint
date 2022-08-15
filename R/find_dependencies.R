#' Find Dependencies Based On Info From [reactlog]
#'
#' Provides functionality to show the user reactive context (reactives, observers,
#' outputs) based on chosen id - reactive context which depends on some id.
#'
#' @param id name of the input or output id.
#' @param binded_filenames_parse_data data.frame with binded parse_data (with new columns: filename and filename_full_path); object returned by `bind_filenames_parse_data`
#' @param reactlog_dependency_df data.frame with info extracted from [reactog] - which reactId depends on which reactId
#' @param all_react_ids all react_ids to be used by `construct_dependency_graph`
#' @param ids_data data.frame with info from [reactlog] - react_id, labels, lines etc. (see `prepare_ids_data` and `prepare_dependency_df_and_ids_data` funs)
#'
#' @return
#' List; for each id (elements of list):
#' data.frame with columns:
#' - filename_full_path
#' - filename
#' - line
#' - src_code: reactive context (reactive, observe or output) source code
#' which belongs as a dependencies to the `id` or
#' NULL if no dependencies found for id.
#' @details
#' User can choose input or output id in the app (the idea is to be able to choose
#' something which is visible) and then, based on this id, this function will find
#' all dependencies, so user can see all relevant parts of code for some input or
#' output.
#' It is necessary to remember, that at the one time user can choose more than
#' one id and these ids can be of type input and output (i.e. different type each id).
#' Thus we need to apply this function for each id and we can't construct dependency
#' graph upfront - we need to construct it for each id from the beginning, because of
#' this different types possible.
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @noRd
find_dependencies <- function(id, binded_filenames_parse_data, reactlog_dependency_df, all_react_ids, ids_data) {
  if (nrow(reactlog_dependency_df) > 0) {
    id_is_input <- ids_data$is_input[ids_data$label == id]
    dependency_graph <- construct_dependency_graph(reactlog_dependency_df, id_is_input)
    ids_data <- dplyr::left_join(ids_data, dependency_graph, by = "react_id")
  }

  graph_group <- ids_data$graph[ids_data$label == id]
  ids_data <- ids_data %>%
    dplyr::filter(.data$graph == graph_group) %>%
    dplyr::filter(!is.na(.data$filename))

  if (nrow(ids_data) > 0) {
    dependencies_src_code <- dplyr::left_join(binded_filenames_parse_data, ids_data[c("filename", "location", "graph")],
                                              by = c("filename" = "filename", "line" = "location"))
    reactives_to_keep <- unique(dependencies_src_code$each_reactive[!is.na(dependencies_src_code$graph)])
    dependencies_src_code <- dependencies_src_code %>%
      dplyr::filter(each_reactive %in% reactives_to_keep) %>%
      dplyr::select(filename_full_path, filename, line, src_code)

    dependencies_src_code
  } else {
    NULL
  }
}

#' Prepare Data From [reactlog] Just Before Searching For Dependencies For Chosen Id
#'
#' @param reactlog_data list returned by [reactlog]
#' @param labelled_observers data.frame returned by `prepare_src_code` with labelled observers
#'
#' @return
#' list with two data.frames:
#' - ids_data - see `prepare_ids_data`
#' - reactlog_dependency_df - see `preapre_reactlog_dependency_df`
#' However, if reactlog_dependency_df has no rows, i.e. in the App no dependencies were
#' found, then we still want to mimic situation where there is no dependencies, so we
#' add column `graph` in this situation indicating that each id belongs to separate group
#' (graph). That way we can assume that `ids_data` always has column `graph`.
#' And one vector:
#' - NULL if no data in reactlog_dependency_df
#' - Or all unique react_ids which then will be used by `construct_dependency_graph`
#' @noRd
prepare_dependency_df_and_ids_data <- function(reactlog_data, labelled_observers) {
  ids_data <- prepare_ids_data(reactlog_data, labelled_observers)
  reactlog_dependency_df <- preapre_reactlog_dependency_df(reactlog_data)
  all_react_ids <- NULL
  if (nrow(reactlog_dependency_df) > 0) {
    # we want to be able to exclude inputs if 'output' id was chosen. We can assume that input can't depends on something else, but only something else depends on input,
    # so we can think that all inputs are in 'depends_on_react_id' column, that's why this join below looks like that. And then (in `construct_dependency_graph`) we can
    # remove all connections to input. This way we would find only objects (reactives) on which output (if chosen id in output) depends on
    reactlog_dependency_df <- dplyr::left_join(reactlog_dependency_df, ids_data[c("react_id", "is_input")], by = c("depends_on_react_id" = "react_id"))
    all_react_ids <- all_react_ids <- unique(c(reactlog_dependency_df$react_id, reactlog_dependency_df$depends_on_react_id))
  } else {
    ids_data <- ids_data %>%
      dplyr::mutate(graph = dplyr::row_number()) # no connections, i.e. mimic no connected graphs
  }

  list(ids_data = ids_data,
       reactlog_dependency_df = reactlog_dependency_df,
       all_react_ids = all_react_ids)

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
#' - label_full: id used in app (as input or output) or label for observer or name for reactive. If
#' input or output - then with `input$` or `output$` at the beginning (that's why it is full label)
#' - filename: basename of file (not a full path!)
#' - location: it is one line in which reactive context (output, reactive, observer) can be find
#' - is_input: if the label is an input, because if output id was chosen, then we will remove inputs before costructing graph
#' - label: label without `input$` or `output$` at the beginning - needed as this is how id is retrieved by JS function
#' Even if no elements in the App, this fun still should return data.frame with 1 row: Theme Counter as label.
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @noRd
prepare_ids_data <- function(reactlog_data, labelled_observers) {
  ids_data <- dplyr::bind_rows(lapply(reactlog_data, extract_ids_data_to_df))
  ids_data <- ids_data %>%
    dplyr::mutate(is_input = grepl("^input\\$", .data$label_full, perl = TRUE),
                  label = gsub("^input\\$|^output\\$", "", .data$label_full, perl = TRUE))
  if (!is.null(labelled_observers)) {
    ids_data <- dplyr::left_join(ids_data, labelled_observers, by = "label")
    ids_data <- ids_data %>%
      mutate(filename = ifelse(is.na(.data$filename), .data$file, .data$filename),
             location = ifelse(is.na(.data$location), .data$location_observer, .data$location)) %>%
      dplyr::select(-c(.data$file, .data$location_observer))
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
#' @importFrom rlang %||%
#' @noRd
extract_ids_data_to_df <- function(reactlog_data) {
  if (reactlog_data$action == "define") {
    data.frame(react_id = reactlog_data$reactId,
               label_full = reactlog_data$label,
               filename = attr(reactlog_data$label, "srcfile") %||% NA,
               location = attr(reactlog_data$label, "srcref")[[1]] %||% NA)
  }
}

#' Extract Info From [reactlog] About Which ReactId Depends On Which ReacId
#'
#' @param reactlog_data list returned by [reactlog]
#'
#' @return
#' data.frame. For each reactId only the direct dependency is showed.
#' So it is necessary to build a whole graph later, which is done by `construct_dependency_graph`.
#' @noRd
prepare_reactlog_dependency_df <- function(reactlog_data) {
  dplyr::bind_rows(lapply(reactlog_data, get_dependencies_from_reactlog))
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
#' @param id_is_input is id an input?
#' @param all_react_ids all reacts_ids which can be found in [reactlog] for 'dependency' action
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
#' other than that. To avoid this, we can just remove inputs from data.frame
#' if the chosen id is of type output - this is safe, because we won't display
#' any parts of code with "inputs" for the user to set breakpoint.
#' However, at the end we want to add missng react_ids, so we can have full data
#' before joining with ids_data. this is especially needed if we have chosen
#' output, so we are removing inputs, but this output depends only on this
#' one input, so is removed entirely from graph and later we would have a problem
#' to find it in ids_data and later as well - after joining ids_data with parse_data.
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @noRd
construct_dependency_graph <- function(reactlog_dependency_df, id_is_input, all_react_ids) {
  if (!id_is_input) {
    reactlog_dependency_df <- reactlog_dependency_df %>%
      dplyr::filter(!.data$is_input)
  }

  reactlog_dependency_df <- reactlog_dependency_df %>%
    dplyr::select(-.data$is_input)

  graph_as_data_frame <- igraph::graph_from_data_frame(reactlog_dependency_df) %>%
    igraph::components() %>%
    igraph::membership() %>%
    utils::stack()
  names(graph_as_data_frame) <- c("graph", "react_id")
  graph_as_data_frame$react_id <- as.character(graph_as_data_frame$react_id)

  if (length(graph_as_data_frame$react_id) < length(all_react_ids)) {
    start_from <- max(graph_as_data_frame$graph) + 1
    missing_react_ids <- all_react_ids[!all_react_ids %in% graph_as_data_frame$react_id]
    graph_as_data_frame <- dplyr::bind_rows(graph_as_data_frame,
                                            data.frame(graph = seq.int(start_from, length(missing_react_ids) + start_from - 1),
                                                       react_id = missing_react_ids))
  }

  graph_as_data_frame
}

#' Bind parse_data Into One data.frame And Divide Each Reactive Context Into Separate Groups
#'
#' @param filenames_parse_data returned by `prepare_src_code`
#'
#' @return
#' All parse data (previously stored in list for separate files) are now
#' binded into one data.frame to hopefully speed up retrieving dependencies.
#' Additionally, we are making groups for each reactive block to later
#' retrieve blocks which depends on Id.
#' @noRd
prepare_filenames_parse_data <- function(filenames_parse_data) {
  filenames_parse_data$parse_data <- mapply(add_filenames,
                                            filenames_parse_data$filename,
                                            filenames_parse_data$filename_full_path,
                                            filenames_parse_data$parse_data,
                                            SIMPLIFY = FALSE,
                                            USE.NAMES = FALSE)
  binded_filenames_parse_data <- dplyr::bind_rows(filenames_parse_data$parse_data)

  binded_filenames_parse_data <- binded_filenames_parse_data %>%
    dplyr::mutate(each_reactive = cumsum(dplyr::lag(is.na(line), default = TRUE))) # each lines for reactive (block divided by NA) in separate group, bottom NA belongs to reactive

  binded_filenames_parse_data
}

#' Add Basename and Full Path to Each parse_data data.frame
#'
#' Helper function for `prepare_filenames_parse_data`
#'
#' @param filename base name of file
#' @param filename_full_path full path to file
#' @param parse_data data.frame with parse data for each file
#'
#' @return
#' Modified filenames_parse_data (originally returned by `prepare_src_code`);
#' new columns are added - with basename and with full path to file
#' @noRd
add_filenames <- function(filename, filename_full_path, parse_data) {
  parse_data$filename <- filename
  parse_data$filename_full_path <- filename_full_path
  parse_data
}
