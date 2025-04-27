#' spending Dataset
#'
#' A dataset containing transaction data combined from: credit card statements,
#' a checking account statement, and payroll deductions.
#' This data is an output of the statementCombine() function.
#'
#' @format A data frame with 5743 rows and 5 variables:
#' \describe{
#'   \item{Date}{Date in format YYYY-MM-DD}
#'   \item{Merchant}{Where the transaction was made}
#'   \item{Category}{Spending category}
#'   \item{Note}{Note regarding transaction}
#'   \item{Amount}{Transaction total}
#' }
#' @source Compiled from my personal finance data
"spending"
