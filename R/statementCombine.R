#' Combine Multiple Statement CSV Files
#'
#' This function reads and combines multiple financial statement CSV files
#' into a single data frame. It ensures the `Date` column is properly formatted
#' and orders the data chronologically.
#'
#' @param files A character vector containing file paths to CSV files.
#' @return A data frame with combined rows, with the `Date` column formatted
#'   as `Date` and ordered chronologically.
#' @keywords internal
#' @import dplyr
#' @export

statementCombine <- function(files) {
  Date <- NULL # Make CMD check stop complaining

  # Read and combine all files
  combined_df <- do.call(rbind, lapply(files, read.csv))

  # Ensure Date column is in Date format
  combined_df$Date <- as.Date(combined_df$Date, format = "%m/%d/%Y")

  # Order by Date
  `%>%` <- dplyr::`%>%`# make CMD check stop complaining
  combined_df <- combined_df %>% dplyr::arrange(Date)

  return(combined_df)
}
