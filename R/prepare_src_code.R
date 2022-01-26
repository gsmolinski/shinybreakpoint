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

find_left_reactives <- function(parse_data) {
  parse_data <- lapply(parse_data, find_direct_parent_id)
  parse_data <- lapply(parse_data, remove_nested_reactives)
  which_null <- which(vapply(parse_data, is.null, FUN.VALUE = logical(1)))
  list(parse_data = parse_data,
       which_null = which_null)
}

find_direct_parent_id <- function(one_parse_data) {
  extracted_lines <- one_parse_data$line1[grep("^reactive$|^eventReactive$|^observe$|^observeEvent$|^render[A-Z]+",
                                             one_parse_data$text, perl = TRUE)]
  if (length(extracted_lines) > 0) {
    first_occurence_of_line <- match(extracted_lines, one_parse_data$line1)
    one_parse_data[first_occurence_of_line, ]
  } else {
    NULL
  }
}

remove_nested_reactives <- function(one_parse_data) {
  if (!is.null(one_parse_data)) {
    shifted_line2 <- dplyr::lag(one_parse_data$line2, n = 1)
    indices <- seq_along(one_parse_data$line2)
    one_parse_data <- one_parse_data %>%
      dplyr::mutate(nested = vapply(indices, is_nested_reactive, FUN.VALUE = logical(1),
                                    line2 = .data$line2,
                                    shifted_line2 = shifted_line2)) %>%
      dplyr::filter(!.data$nested)

    one_parse_data
  }
}

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
