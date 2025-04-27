#' deposits Dataset
#'
#' A dataset containing deposit transactions made into a checking account.
#' The deposit data also contains entries for employer contributions to
#' retirement accounts so that the deductions are properly accounted for.
#'
#' @format A data frame with 5743 rows and 5 variables:
#' \describe{
#'   \item{Date}{Date in format YYYY-MM-DD}
#'   \item{Merchant}{Deposit source}
#'   \item{Category}{Deposit category}
#'   \item{Note}{Note regarding deposit}
#'   \item{Amount}{Deposit total}
#' }
#' @source Compiled from my personal finance data
"deposits"
