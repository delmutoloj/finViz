#' Create a Financial Sankey Diagram
#'
#' This function generates an interactive Sankey diagram to visualize income
#' allocation and spending patterns. It dynamically determines income
#' categories from the deposit data and structures financial transactions into
#' a hierarchical flow, showing how income is distributed across expense
#' categories and vendors.
#'
#' @param deposits A data frame containing deposit transactions with columns:
#'   `Category` and `Amount`.
#' @param spending A data frame containing spending transactions with columns:
#'   `Category`, `Merchant`, and `Amount`.
#' @return An interactive Sankey diagram (`networkD3` object) displaying
#'   financial flows.
#' @keywords internal
#' @import dplyr
#' @import networkD3
#' @import htmlwidgets
#' @export

# Dynamic income category naming
financialSankey <- function(deposits, spending) {

  # make CMD check stop complaining
  `%>%` <- dplyr::`%>%`
  Category <- Amount <- Merchant <- Source <- Target <- Date <- NULL

  # Dynamically determine income categories
  income_categories <- unique(deposits$Category)

  # Summarize total income by category
  income_totals_by_category <- deposits %>%
    dplyr::group_by(Category) %>%
    dplyr::summarise(Amount = sum(Amount))

  income_total <- sum(income_totals_by_category$Amount)  # Total income

  # Summarize total expenses by category
  expense_summary <- spending %>%
    dplyr::group_by(Category) %>%
    dplyr::summarise(Amount = sum(Amount))

  # Summarize expenses per vendor within each category
  vendor_summary <- spending %>%
    dplyr::group_by(Category, Merchant) %>%
    dplyr::summarise(Amount = sum(Amount))

  # Create a list of unique nodes in hierarchical order
  nodes <- data.frame(name = c(income_totals_by_category$Category, "Income",
                               expense_summary$Category,
                               unique(spending$Merchant)))

  # Function to get the index of a node
  get_index <- function(name) {
    match(name, nodes$name) - 1  # Convert to zero-based index for networkD3
  }

  # Create links for income categories → "Income"
  links <- data.frame(
    Source = sapply(income_totals_by_category$Category, get_index),
    Target = rep(get_index("Income"), nrow(income_totals_by_category)),
    Value = income_totals_by_category$Amount
  )

  # Add links for "Income" → Expense categories
  category_indices <- sapply(expense_summary$Category, get_index)
  links <- rbind(
    links,
    data.frame(Source = get_index("Income"),
               Target = category_indices,
               Value = expense_summary$Amount)
  )

  # Add links for each category → vendor (Ensure all columns match)
  vendor_links <- vendor_summary %>%
    dplyr::mutate(Source = sapply(Category, get_index),
           Target = sapply(Merchant, get_index)) %>%
    dplyr::select(Source, Target, Value = Amount)

  # Ensure vendor_links has the same structure as links
  vendor_links <- vendor_links[, c("Source", "Target", "Value")]

  # Combine all links
  links <- rbind(links, vendor_links)

  # Create Sankey Diagram
  sankey <- networkD3::sankeyNetwork(
    Links = links,
    Nodes = nodes,
    Source = "Source",
    Target = "Target",
    Value = "Value",
    NodeID = "name",
    fontSize = 14,
    nodeWidth = 30
  )

  # Add value labels
  sankey <- htmlwidgets::onRender(
    x = sankey,
    jsCode = '
      function(el, x){
        d3.select(el).selectAll(".node text")
          .text(d => d.name + " (" + d3.format("(.0f")(d.value) + ")");
      }
    '
  )

  return(sankey)
}
