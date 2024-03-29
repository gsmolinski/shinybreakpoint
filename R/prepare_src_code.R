#' Return Prepared Source Code and Environments
#'
#' @param caller_env object returned by 'rlang::caller_env()', passed by exported function, i.e.
#' function used directly by user.
#'
#' @return
#' Named list:
#' 'filenames_parse_data': data.frame with full path to file, file name and parse data,
#' but only if file contained at least one reactive function and this function wasn't live in
#' Global Environment (because we have removed Global Environment previously from search path).
#' 'labelled_reactive_objects': data.frame returned by `get_labelled_reactive_objects`. This is needed to filter
#' dependencies based on chosen Id, because in some circumstances there is no source reference in reactlog for reactive objects.
#' 'envirs': list of environments returned by `collect_filenames_parse_data`
#' @noRd
prepare_src_code <- function(caller_env) {
  filenames_parse_data_env <- collect_filenames_parse_data(caller_env)
  if (!is.null(filenames_parse_data_env)) {
    envirs <- filenames_parse_data_env$envirs
    filenames_parse_data <- filenames_parse_data_env$filenames_parse_data

    parse_data_all <- dplyr::bind_rows(filenames_parse_data$parse_data, .id = "filename_full_path")
    parse_data_all$filename_full_path <- filenames_parse_data$filename_full_path[as.numeric(parse_data_all$filename_full_path)]

    find_left_reactives_result <- find_left_reactives(filenames_parse_data$parse_data)
    filenames_parse_data$parse_data <- find_left_reactives_result$parse_data
    if (length(find_left_reactives_result$which_null) > 0) {
      filenames_parse_data <- filenames_parse_data[-find_left_reactives_result$which_null, ]
      envirs <- envirs[-find_left_reactives_result$which_null]
    }

    filenames_parse_data$parse_data <- lapply(filenames_parse_data$parse_data, retrieve_src_code)

    labelled_reactive_objects <- get_labelled_reactive_objects(parse_data_all, filenames_parse_data)

    list(filenames_parse_data = filenames_parse_data,
         labelled_reactive_objects = labelled_reactive_objects,
         envirs = envirs)
  }
}

#' Find and Left Reactives
#'
#' Removes not reactives functions from parse_data.
#'
#' @param parse_data one data.frame with parse data.
#'
#' @return
#' Named list:
#' 'parse_data': data.frame for each path (file) with exprs (top id) contain reactive fun
#' or NULL if no exprs contain reactive fun.
#' 'which_null': indices where are NULLs in returning list.
#' @noRd
find_left_reactives <- function(parse_data) {
  parse_data <- lapply(parse_data, find_direct_parent_id_with_reactive)
  parse_data <- lapply(parse_data, remove_nested_reactives)
  which_null <- which(vapply(parse_data, is.null, FUN.VALUE = logical(1)))
  list(parse_data = parse_data,
       which_null = which_null)
}

#' Find Expressions Contain Reactives
#'
#' @param one_parse_data one data.frame with parse data.
#'
#' @return
#' data.frame with parse data, but only with exprs contain reactives
#' nested in named functions. NULL if nothing found.
#' @details
#' utils::getParseData with argument 'NA' passed to parameter 'includeText'
#' does not return text for 'expr' token, but we will use later function utils::getParseText
#' to retrieve these texts. However, it is needed to find top id (for expr), so we are sure
#' we have whole reactive function. Also, 'find_direct_parent_id_with_reactive' returns
#' only exprs which are reactives nested in named functions. To find reactives, regex is used -
#' this regex may need to be updated in next 'Shiny' versions and should be taken into account
#' that other packages can export their reactives and should be find by this function as well.
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @noRd
find_direct_parent_id_with_reactive <- function(one_parse_data) {
  funs_one_parse_data <- one_parse_data %>%
    dplyr::filter(.data$token == "SYMBOL_FUNCTION_CALL")
  extracted_lines <- funs_one_parse_data$line1[grep(get_reactive_context_regex(),
                                                    funs_one_parse_data$text, perl = TRUE)]
  if (length(extracted_lines) > 0) {
    named_funs_lines <- find_lines_with_named_funs(one_parse_data)
    first_occurence_of_line <- match(extracted_lines, one_parse_data$line1)
    one_parse_data <- one_parse_data[first_occurence_of_line, ]
    reactives_in_named_funs <- vapply(one_parse_data$line1, function(e) any(mapply(dplyr::between,
                                                                               left = named_funs_lines$line1,
                                                                               right = named_funs_lines$line2,
                                                                               MoreArgs = list(x = e),
                                                                               USE.NAMES = FALSE)),
                                      FUN.VALUE = logical(1))
    one_parse_data[reactives_in_named_funs, ]
  } else {
    NULL
  }
}

#' Find Starting and Ending Line with Named Function Definition
#'
#' @param one_parse_data one data.frame with parse data.
#'
#' @return data.frame with columns: line1, line2.
#' Line1 is starting line, line2 is ending line.
#' @details
#' The idea is that later we would restore fun body, but only for named
#' functions (after using 'parse()' on specific file), so we need to have
#' only reactives nested in functions - and this function finds named functions.
#' We will use lines returned by this function to check later if reactive is between
#' those lines - if yes, it means that reactive lives inside named function and thus
#' can be shown to the user as possible place to put 'browser()', i.e. we want to
#' avoid situation where user would like to try to put 'browser()' where it is
#' impossible, because 'does_breakpoint_can_be_set()' will be FALSE.
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @noRd
find_lines_with_named_funs <- function(one_parse_data) {
  id_parent_token <- one_parse_data %>%
    dplyr::filter(.data$token %in% c("FUNCTION", "expr")) %>%
    dplyr::select("id", "parent", "token")

  top_ids <- vapply(id_parent_token$parent[id_parent_token$token == "FUNCTION"],
                    find_top_expr,
                    FUN.VALUE = integer(1),
                    id_parent_token = id_parent_token)

  # needed in case there were functions nested in functions
  top_ids <- unique(top_ids)

  exprs <- lapply(top_ids, function(x) str2lang(utils::getParseText(one_parse_data, x)))
  names(exprs) <- top_ids
  named_funs_ids <- Filter(is_named_fun, exprs)

  named_funs_lines <- one_parse_data %>%
    dplyr::filter(.data$id %in% as.integer(names(named_funs_ids))) %>%
    dplyr::select("line1", "line2")

  named_funs_lines
}

#' Find Top ID of Expr Which Is Basically a Function
#'
#' Find and get id of expression which should be whole function definition.
#'
#' @param parent parent column in data.frame returned by utils::getParseData.
#' @param id_parent_token parse_data with columns: id, parent, token.
#'
#' @return top id.
#' @details
#' It is necessary to get whole definition of each function (i.e. 'FUNCTION' token)
#' to check later if this is named function or not.
#' @noRd
find_top_expr <- function(parent, id_parent_token) {
  id <- vector("integer", 1L)
  while (parent != 0) {
    id <- parent
    parent <- id_parent_token$parent[id_parent_token$id == id]
  }
  id
}

#' Remove Nested Reactives
#'
#' @param one_parse_data one data.frame with parse data or NULL if no parse data.
#'
#' @return
#' Parse data (data.frame) without exprs which were nested or NULL if NULL was passed into.
#' @noRd
remove_nested_reactives <- function(one_parse_data) {
  if (!is.null(one_parse_data)) {
    shifted_line2 <- dplyr::lag(one_parse_data$line2, n = 1)
    indices <- seq_along(one_parse_data$line2)
    nested <- vapply(indices, is_nested_reactive, FUN.VALUE = logical(1),
                     line2 = one_parse_data$line2,
                     shifted_line2 = shifted_line2)
    nested <- which(nested)
    if (length(nested) > 0) {
      one_parse_data <- one_parse_data[-nested, ]
    }

    one_parse_data
  }
}

#' Check if Reactive Is Nested
#'
#' @param indice one indice of column 'line2' from parse data.
#' @param line2 whole column 'line2' from parse data (last line of each expression).
#' @param shifted_line2 whole column 'line2' from parse data, but modified by dplyr::lag.
#'
#' @return
#' TRUE if reactive is nested in any other reactive, otherwise FALSE.
#' @noRd
is_nested_reactive <- function(indice, line2, shifted_line2) {
  line2 <- line2[[indice]]
  shifted_line2 <- shifted_line2[seq_len(indice)]
  if (length(shifted_line2) == 1) { # first reactive, so won't be nested
    FALSE
  } else {
    shifted_line2 <- shifted_line2[-1] # remove NA
    nested <- ifelse(line2 < shifted_line2, TRUE, FALSE)
    if (any(nested)) {
      TRUE
    } else {
      FALSE
    }
  }
}

#' Find And Get Only Labelled Reactive Objects
#'
#' Needed for reactlog to find dependencies
#'
#' @param parse_data_all data.frame with parse data from all
#' files as well as a column with full path to file, but only
#' with reactive context kept.
#' @param filenames_parse_data parse data after checked for
#' reactives.
#'
#' @return
#' data.frame with only labelled reactive objects and only if
#' string was used for label, not variable; columns:
#' - location_object: one line from reactive object
#' - label
#' - file (only basename for path, because in [reactlog] is only basename for reactives and outputs)
#' Or NULL if nothing found
#' @details
#' All rules for reactive context apply here as well -
#' so only reactive context (reactive objects) nested in named function.
#' It returns only reactive objects labelled using only string (not e.g. variable),
#' because reactlog can resolve variable or function, but we can't by reading
#' just a source code.
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @noRd
get_labelled_reactive_objects <- function(parse_data_all, filenames_parse_data) {
  parent_id_filename <- parse_data_all %>%
    dplyr::filter(.data$filename_full_path %in% filenames_parse_data$filename_full_path &
                    .data$token == "SYMBOL_FUNCTION_CALL") %>%
    dplyr::filter(grepl(get_labelled_objects_regex(),
                        .data$text, perl = TRUE)) %>%
    dplyr::select(id = "parent", "filename_full_path")

  if (nrow(parent_id_filename) > 0) {
    expr_id_filename <- parent_id_filename %>%
      dplyr::left_join(parse_data_all[c("id", "parent", "filename_full_path")],
                       by = c("id", "filename_full_path")) %>%
      dplyr::select("parent", "filename_full_path")

    labels_or_na <- mapply(extract_label, parent_id = expr_id_filename$parent, filename_full_path = expr_id_filename$filename_full_path,
                           MoreArgs = list(parse_data_all = parse_data_all), SIMPLIFY = FALSE, USE.NAMES = FALSE)

    expr_lines_label_filename <- expr_id_filename %>%
      dplyr::mutate(label = gsub('"', "", unlist(labels_or_na, use.names = FALSE), fixed = TRUE)) %>% # remove double quotes
      dplyr::filter(!is.na(.data$label)) %>%
      dplyr::rename(id = "parent") %>%
      dplyr::left_join(parse_data_all, by = c("id", "filename_full_path")) %>%
      dplyr::mutate(file = basename(.data$filename_full_path)) %>%
      dplyr::select(location_object = "line1", "label", "file")

    if (nrow(expr_lines_label_filename) > 0) {
      expr_lines_label_filename
    } else {
      NULL
    }
  } else {
    NULL
  }
}

#' Helper for `get_labelled_reactive_objects`
#'
#' Search if reactive object is labelled by only string
#'
#' @param parent_id id for reactive object
#' @param filename_full_path filename where reactive object exists
#' @param parse_data_all all parse data from all files
#'
#' @return
#' label text if string for reactive object or NA
#' @noRd
extract_label <- function(parent_id, filename_full_path, parse_data_all) {
  indice_label <- which(parse_data_all$parent == parent_id &
                        parse_data_all$token == "SYMBOL_SUB" &
                        parse_data_all$text == "label" &
                        parse_data_all$filename_full_path == filename_full_path)
  if (length(indice_label) > 0) { # for integer(0) cases, but should be length 1 exactly
    if (tryCatch(parse_data_all[indice_label + 1, "token"] == "EQ_SUB", error = function() FALSE) &
        tryCatch(parse_data_all[indice_label + 2, "token"] == "STR_CONST", error = function() FALSE)) {
      parse_data_all[indice_label + 2, "text"]
    } else {
      NA_character_
    }
  } else {
    NA_character_
  }
}

#' Get Text for Expr and Format It
#'
#' Source code is retrieved using utils::getParseText and then splitted to separate lines.
#' Line indicators are added as well and lines containing only comments are removed.
#'
#' @param one_parse_data one data.frame with parse data.
#'
#' @return
#' data.frame with col 'line' where is line number for source code
#' and col 'src_code' with formatted source code.
#' @details
#' Each block of code is separated by empty row (NA in both columns). This is only
#' for readability and empty row is added also if is not present in an original
#' source code, i.e. in file.
#' @importFrom rlang .data
#' @noRd
retrieve_src_code <- function(one_parse_data) {
  lines <- seq_vec(one_parse_data$line1, one_parse_data$line2)
  lines <- lapply(lines, append, values = NA_integer_)
  lines <- unlist(lines, use.names = FALSE)
  parse_data <- utils::getParseText(one_parse_data, one_parse_data$id)
  parse_data <- strsplit(parse_data, split = "\n", fixed = TRUE)
  parse_data <- lapply(parse_data, append, values = NA_character_)
  parse_data <- unlist(parse_data, use.names = FALSE)

  line_src_code <- data.frame(line = lines,
                              src_code = parse_data)

  line_src_code
}

# vectorized version of seq, returns list
seq_vec <- Vectorize(seq.default, vectorize.args = c("from", "to"), SIMPLIFY = FALSE)
