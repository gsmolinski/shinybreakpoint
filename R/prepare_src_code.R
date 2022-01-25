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
    shifted_line1 <- dplyr::lead(one_parse_data$line1, n = 1)
    one_parse_data <- one_parse_data %>%
      # if NA than it's last started line, so won't be nested
      mutate(nested = ifelse(line2 < shifted_line1 | is.na(shifted_line1), FALSE, TRUE)) %>%
      filter(!nested)
    one_parse_data
  }
}

retrieve_src_code <- function(one_parse_data) {

}
