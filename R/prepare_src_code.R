#' Return Prepared Source Code and Environments
#'
#' @param caller_env object returned by 'rlang::caller_env()', passed by exported function, i.e.
#' function used directly by user.
#'
#' @return
#' Named list:
#' 'filenames_parse_data': data.frame with full path to file, file name and parse data,
#' but only if file contained at least one reactive funtion and this funtion wasn't live in
#' Global Environment (because we have removed Global Environment previously from search path).
#' 'envirs': named list of environments for each file.
#' @noRd
prepare_src_code <- function(caller_env) {
  filenames_parse_data_env <- collect_filenames_parse_data(caller_env)
  if (!is.null(filenames_parse_data)) {
    envirs <- filenames_parse_data_env$envirs
    filenames_parse_data <- filenames_parse_data_env$filenames_parse_data

    find_left_reactives_result <- find_left_reactives(filenames_parse_data$parse_data)
    filenames_parse_data_env$filenames_parse_data <- find_left_reactives_result$parse_data
    if (length(find_left_reactives_result$which_null) > 0) {
      filenames_parse_data <- filenames_parse_data[-find_left_reactives_result$which_null, ]
      envirs <- envirs[-find_left_reactives_result$which_null]
    }
    filenames_parse_data$parse_data <- lapply(filenames_parse_data$parse_data, retrieve_src_code)

    list(filenames_parse_data = filenames_parse_data,
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
#' if at least one reactive was found, otherwise NULL.
#' @details
#' utils::getParseData with argument 'NA' passed to parameter 'includeText'
#' does not return text for 'expr' token, but we will use later function utils::getParseText
#' to retrieve these texts. However, it is needed to find top id (for expr), so we are sure
#' we have whole reactive function. Also, 'find_direct_parent_id_with_reactive' returns
#' only exprs which are reactives. To find reactives, regex is used - this regex may need to be
#' updated in next 'Shiny' versions and should be taken into account that other packages
#' can export their reactives and should be find by this function as well.
#' @noRd
find_direct_parent_id_with_reactive <- function(one_parse_data) {
  extracted_lines <- one_parse_data$line1[grep("^reactive$|^eventReactive$|^observe$|^observeEvent$|^render[A-Z]+",
                                             one_parse_data$text, perl = TRUE)]
  if (length(extracted_lines) > 0) {
    first_occurence_of_line <- match(extracted_lines, one_parse_data$line1)
    one_parse_data[first_occurence_of_line, ]
  } else {
    NULL
  }
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
    one_parse_data <- one_parse_data[-nested, ]

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

#' Get Text for Expr and Format It
#'
#' Source code is retrieved using utils::getParseText and then splitted to separate lines.
#' Line indicators are added as well.
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
#' @noRd
retrieve_src_code <- function(one_parse_data) {
  lines <- seq_vec(one_parse_data$line1, one_parse_data$line2)
  lines[-length(lines)] <- lapply(lines[-length(lines)], append, values = NA_integer_)
  lines <- unlist(lines, use.names = FALSE)
  parse_data <- utils::getParseText(one_parse_data, one_parse_data$id)
  parse_data <- strsplit(parse_data, split = "\n", fixed = TRUE)
  parse_data[-length(parse_data)] <- lapply(parse_data[-length(parse_data)], append,
                                            values = NA_character_)
  parse_data <- unlist(parse_data, use.names = FALSE)

  data.frame(line = lines,
             src_code = parse_data)
}

# vectorized version of seq, returns list
seq_vec <- Vectorize(seq.default, vectorize.args = c("from", "to"), SIMPLIFY = FALSE)
