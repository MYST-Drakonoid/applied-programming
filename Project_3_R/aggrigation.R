
library(lubridate)
library(jsonlite)
library(dplyr)
library(stringr)


aggregation_general <- function(dataset) {


  aggregate_tables <- list()

  sales <- dataset %>%
    mutate(
      OrderDate = lubridate::ymd_hms(OrderDate),  # Convert string to datetime
      Year      = lubridate::year(OrderDate),    # Extract year
      Month     = lubridate::month(OrderDate),   # Extract month number
      Quarter   = lubridate::quarter(OrderDate), # Extract quarter
      Weekday   = lubridate::wday(OrderDate, label = TRUE) # Extract weekday name
    )

  aggregate_sales <- function(data, ...) {
    summary <- data %>%
      group_by(...) %>%
      summarise(
        TotalQuantity = sum(Quantity, na.rm = TRUE),      # total units sold
        TotalRevenue  = sum(Price, na.rm = TRUE),    # total revenue
        AvgPrice      = mean(Price, na.rm = TRUE),   # average price per sale
        MedianPrice   = median(Price, na.rm = TRUE), # median price per sale
        OrdersCount   = n(),                               # number of orders
        .groups = "drop"
      )
    return(summary)
  }

aggregate_tables <- list(
  item_summary          = aggregate_sales(sales, Item),
  customer_summary      = aggregate_sales(sales, Customer),
  item_customer_summary = aggregate_sales(sales, Item, Customer),
  monthly_summary       = aggregate_sales(sales, Year, Month),
  customer_month_sum    = aggregate_sales(sales, Customer, Year, Month)
)


  return(aggregate_tables)
}