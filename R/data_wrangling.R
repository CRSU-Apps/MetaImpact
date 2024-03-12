
#' Remove leading and trailing whitespace and collapse multiple whitespace characters between words.
#' 
#' @param data Data frame to clean
#' @return Cleaned data frame
CleanData <- function(data) {
  return(dplyr::mutate(data, across(where(is.character), stringr::str_squish)))
}

#' Rename the columns of a data frame to match the expected letter casing.
#'
#' @param data Data frame to fix
#'
#' @return Data frame with renamed columns.
.FixColumnNameCases <- function(data) {
  corrected_names <- unlist(
    sapply(
      names(data),
      function (name) {
        return(.CorrectColumnName(name, meta_impact_column_names))
      }
    )
  )
  
  names(data) <- corrected_names
  return(data)
}

#' Correct a column name to match the expected letter casing.
#'
#' @param original_name Column name to fix
#' @param column_names Named vector where each name is a regular expression to match, and the value is the replacement string.
#'
#' @return The corrected column name.
.CorrectColumnName <- function(original_name, column_names) {
  matches <- unlist(
    sapply(
      column_names$pattern,
      function(pattern) {
        if (length(grep(pattern, original_name)) > 0) {
          column_names$replacement[column_names$pattern == pattern]
        } else {
          NULL
        }
      }
    )
  )
  
  if (length(matches) > 0) {
    return(
      sub(
        names(matches)[1],
        matches[1],
        original_name
      )
    )
  }
  
  return(original_name)
}

#' Add a new column in the data for study IDs, both for long and wide formats.
#' 
#' @param data Data frame in which to search for treatment names
#' @return Vector of all treatment names
.AddStudyIds <- function(data) {
  study_names <- unique(data$Study)
  
  # Add study IDs to data frame
  # data$StudyID <- match(data$Study, study_names)
  data <- cbind(StudyID = match(data$Study, study_names), data)
  
  return(data)
}

#' Wrangle the uploaded data into a form usable by the internals of the app, both for long and wide formats.
#' 
#' @param data Data frame to wrangle
#' @return Data frame which is usable by the rest of the app
WrangleUploadData <- function(data) {
  new_df <- data %>%
    .FixColumnNameCases() %>%
    .AddStudyIds()
  
  return(new_df)
}
