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
financialSankey <- function(deposits, spending) {
  # Make CMD check stop complaining about undefined variables
  `%>%` <- dplyr::`%>%`
  Category <- Amount <- Merchant <- Source <- Target <- Value <-  NULL

  # Define deduction categories
  deduction_cats <- c("Taxes", "Insurance", "Retirement")

  # Summarize deposits by Category
  deposit_summary <- deposits %>%
    dplyr::group_by(Category) %>%
    dplyr::summarise(Amount = sum(Amount), .groups = "drop")

  gross_income <- sum(deposit_summary$Amount)

  # Summarize spending by Category and Merchant
  spending_summary <- spending %>%
    dplyr::group_by(Category, Merchant) %>%
    dplyr::summarise(Amount = sum(Amount), .groups = "drop")

  # Total Deductions
  deductions_summary <- spending %>%
    dplyr::filter(Category %in% deduction_cats) %>%
    dplyr::group_by(Category) %>%
    dplyr::summarise(Amount = sum(Amount), .groups = "drop")

  total_deductions <- sum(deductions_summary$Amount)
  net_income <- gross_income - total_deductions

  # Other (non-deduction) spending categories
  net_spending_summary <- spending %>%
    dplyr::filter(!Category %in% deduction_cats) %>%
    dplyr::group_by(Category) %>%
    dplyr::summarise(Amount = sum(Amount), .groups = "drop")

  # Build list of unique nodes
  nodes <- data.frame(name = c(
    deposit_summary$Category,  # Dynamic income categories
    "Gross Income",
    "Net Income",
    "Deductions",
    deduction_cats,  # Fixed deduction categories
    unique(net_spending_summary$Category),  # Dynamic spending categories
    unique(spending$Merchant)  # All vendors
  ), stringsAsFactors = FALSE)

  # Helper to get node index (0-based)
  get_index <- function(name) match(name, nodes$name) - 1

  # 1. Income Category → Gross Income
  links <- deposit_summary %>%
    dplyr::mutate(Source = sapply(Category, get_index),
                  Target = get_index("Gross Income"),
                  Value = Amount) %>%
    dplyr::select(Source, Target, Value)

  # 2. Gross Income → Net Income and Deductions
  links <- rbind(links,
                 data.frame(Source = get_index("Gross Income"),
                            Target = get_index("Net Income"),
                            Value = net_income),
                 data.frame(Source = get_index("Gross Income"),
                            Target = get_index("Deductions"),
                            Value = total_deductions)
  )

  # 3. Deductions → Deduction Categories
  if (nrow(deductions_summary) > 0) {
    deduction_links <- deductions_summary %>%
      dplyr::mutate(Source = get_index("Deductions"),
                    Target = sapply(Category, get_index),
                    Value = Amount) %>%
      dplyr::select(Source, Target, Value)
    links <- rbind(links, deduction_links)
  }

  # 4. Deduction Categories → Vendors
  deduction_vendor_links <- spending_summary %>%
    dplyr::filter(Category %in% deduction_cats) %>%
    dplyr::mutate(Source = sapply(Category, get_index),
                  Target = sapply(Merchant, get_index)) %>%
    dplyr::select(Source, Target, Value = Amount)
  links <- rbind(links, deduction_vendor_links)

  # 5. Net Income → Other Spending Categories
  if (nrow(net_spending_summary) > 0) {
    net_category_links <- net_spending_summary %>%
      dplyr::mutate(Source = get_index("Net Income"),
                    Target = sapply(Category, get_index),
                    Value = Amount) %>%
      dplyr::select(Source, Target, Value)
    links <- rbind(links, net_category_links)
  }

  # 6. Other Spending Categories → Vendors
  net_vendor_links <- spending_summary %>%
    dplyr::filter(!Category %in% deduction_cats) %>%
    dplyr::mutate(Source = sapply(Category, get_index),
                  Target = sapply(Merchant, get_index)) %>%
    dplyr::select(Source, Target, Value = Amount)
  links <- rbind(links, net_vendor_links)

  # Create Sankey diagram
  sankey <- networkD3::sankeyNetwork(
    Links = links,
    Nodes = nodes,
    Source = "Source",
    Target = "Target",
    Value = "Value",
    NodeID = "name",
    fontSize = 18,
    nodeWidth = 25,
    iterations = 50 # Add more iterations to avoid overlapping
  )

  # Add value labels to nodes
  sankey <- htmlwidgets::onRender(
    sankey,
    jsCode = '
      function(el, x){
        d3.select(el).selectAll(".node text")
          .text(d => d.name + " (" + d3.format("(.0f")(d.value) + ")");
      }
    '
  )

  return(sankey)
}

