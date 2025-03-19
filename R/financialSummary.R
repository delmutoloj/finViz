#' Create a Summary of Financial Data
#'
#' This function generates a summary of financial data, including total income,
#' total expenses, net balance, and aggregated amounts by category and merchant.
#'
#' @param deposits A data frame containing deposit transactions with columns:
#'   `Category` and `Amount`.
#' @param spending A data frame containing spending transactions with columns:
#'   `Merchant` and `Amount`.
#' @return Prints a summary including total income, total expenses, net balance,
#'   and aggregated amounts by category and merchant.
#' @keywords internal
#' @export

financialSummary <- function(deposits, spending) {
  # Calculate total income and total expenses
  total_in <- sum(deposits$Amount, na.rm = TRUE)
  total_out <- sum(spending$Amount, na.rm = TRUE)

  # Calculate the difference between total income and total expenses
  net_balance <- total_in - total_out

  # Aggregate income by category
  spending_by_category <- aggregate(Amount ~ Category, data = spending, sum)
  
  # Sort spending by category largest to smallest
  spending_by_category <- spending_by_category[order(
    spending_by_category$Amount, decreasing = TRUE), ]

  # Aggregate spending by merchant
  spending_by_merchant <- aggregate(Amount ~ Merchant, data = spending, sum)
  
  # Sort spending by merchant largest to smallest
  spending_by_merchant <- spending_by_merchant[order(
    spending_by_merchant$Amount, decreasing = TRUE), ]
  
  # Print the summary
  cat("Total Income: ", total_in, "\n")
  cat("Total Expenses: ", total_out, "\n")
  cat("Net Balance (Income - Expenses): ", net_balance, "\n\n")

  cat("Spending by Category:\n")
  print(spending_by_category)

  cat("\nSpending by Merchant:\n")
  print(spending_by_merchant)
}
