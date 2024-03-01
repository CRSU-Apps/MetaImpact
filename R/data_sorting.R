
#' Sorting algorithm which returns the data frame without modification.
#'
#' @param df Data frame to pass through.
#'
#' @return Unmodified data frame.
NullSort <- function(df) {
  return(df)
}

#' Sort a data frame alphabetically by the contents of the "Study" column.
#'
#' @param df Data frame to sort
#'
#' @return Sorted data frame.
SortByStudyName <- function(df) {
  return(df[order(df$Study), ])
}

#' Sort a data frame by the decreasing contents of the "N" column, summed for all "N" and "N.*" columns for the same study.
#'
#' @param df Data frame to sort
#'
#' @return Sorted data frame.
SortByParticipantCount <- function(df) {
  participants_by_row <- sapply(
    1:nrow(df),
    function(index) {
      .FindParticipantCount(df, df$Study[index])
    }
  )
  
  return(df[order(participants_by_row, decreasing = TRUE), ])
}

#' Find the total participant count for a given study in the data frame.
#'
#' @param df Data frame from which to extract participant count.
#' @param study_name Study name for which to extract participant count.
#'
#' @return Total participant count for given study.
.FindParticipantCount <- function(df, study_name) {
  n_pattern <- meta_impact_column_names$pattern[meta_impact_column_names$name == "N"]
  n_column_indices <- which(stringr::str_detect(string = names(df), pattern = n_pattern))
  
  study_row_indices <- which(df$Study == study_name)
  
  return(sum(df[study_row_indices, n_column_indices]))
}
