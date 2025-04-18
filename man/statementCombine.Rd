% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/statementCombine.R
\name{statementCombine}
\alias{statementCombine}
\title{Combine Multiple Statement CSV Files}
\usage{
statementCombine(files)
}
\arguments{
\item{files}{A character vector containing file paths to CSV files.}
}
\value{
A data frame with combined rows, with the `Date` column formatted
  as `Date` and ordered chronologically.
}
\description{
This function reads and combines multiple financial statement CSV files
into a single data frame. It ensures the `Date` column is properly formatted
and orders the data chronologically.
}
\keyword{internal}
