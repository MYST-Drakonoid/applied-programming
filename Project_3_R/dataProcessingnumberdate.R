
library(lubridate)
library(jsonlite)
library(dplyr)
library(stringr)





number_proc <- function(dataset) {
  dataset %>%
    group_by(Item) %>%
    summarise(
      total_quantity     = sum(Quantity, na.rm = TRUE),
      total_sales        = sum(Price, na.rm = TRUE),
      average_quantity   = mean(Quantity, na.rm = TRUE),
      minimum_sale       = min(Price, na.rm = TRUE),
      maximum_sale       = max(Price, na.rm = TRUE),
      maximum_sold_items = max(Quantity, na.rm = TRUE),
      .groups = "drop"
    )
}

date_proc <- function(dataset) {
  # Create a list to store all processed tables
  date_tables <- list()

  # ----------------------------
  # Step 1: Convert OrderDate to usable components
  # ----------------------------
  sales <- dataset %>%
    mutate(
      OrderDate = lubridate::ymd_hms(OrderDate),  # Convert string to datetime
      Year      = lubridate::year(OrderDate),    # Extract year
      Month     = lubridate::month(OrderDate),   # Extract month number
      Quarter   = lubridate::quarter(OrderDate), # Extract quarter
      Weekday   = lubridate::wday(OrderDate, label = TRUE) # Extract weekday name
    )

  # ----------------------------
  # Step 2: Aggregate monthly sales per item
  # ----------------------------
  monthly_sales <- sales %>%
    group_by(Item, Year, Month) %>%
    summarise(
      total_monthly_sales = sum(Price, na.rm = TRUE),  # Sum sales for the month
      total_quantity      = sum(Quantity, na.rm = TRUE), # Sum quantity sold
      .groups = "drop"  # Ungroup after summarise
    )

  # Store in the date_tables list
  date_tables[["monthly_sales"]] <- monthly_sales

  # ----------------------------
  # Step 3: Aggregate quarterly sales per item
  # ----------------------------
  quarterly_sales <- sales %>%
    group_by(Item, Year, Quarter) %>%
    summarise(
      total_quarterly_sales = sum(Price, na.rm = TRUE),
      total_quantity        = sum(Quantity, na.rm = TRUE),
      .groups = "drop"
    )

  date_tables[["quarterly_sales"]] <- quarterly_sales

  # ----------------------------
  # Step 4: Function to get the top-performing metric (weekday, month, quarter)
  # ----------------------------
  best_sale <- function(data, metric) {
    best <- data %>%
      group_by({{metric}}) %>%  # Dynamic grouping using the metric column
      summarise(
        total_sales = sum(TotalPrice, na.rm = TRUE),  # Sum of total sales
        .groups = "drop"
      ) %>%
      arrange(desc(total_sales)) %>%  # Sort descending
      slice(1)  # Keep only the top-performing row
    return(best)
  }

  # ----------------------------
  # Step 5: Get top metrics for weekday, month, quarter
  # ----------------------------
  best_weekday <- best_sale(sales, Weekday)
  best_month   <- best_sale(sales, Month)
  best_quarter <- best_sale(sales, Quarter)

  # Combine into a single table with a label for each metric type
  best_summary <- bind_rows(
    best_weekday %>% mutate(metric_type = "Weekday"),
    best_month   %>% mutate(metric_type = "Month"),
    best_quarter %>% mutate(metric_type = "Quarter")
  )

  # Store the best_summary in the list
  date_tables[["best_summary"]] <- best_summary

  # ----------------------------
  # Step 6: cummulative goals
  # ----------------------------

  cumulative_sales <- function(data, ...) {
    cumulative <- data %>%
      group_by(...) %>%  # group by provided columns
      summarise(
        period_sales = sum(Price, na.rm = TRUE), # sum for each period
        .groups = "drop"
      ) %>%
      arrange(...) %>% # sort by same columns
      mutate(cumulative_sales = cumsum(period_sales))  # running total
    return(cumulative)
  }

  # Generate cumulative totals
  daily_cumulative      <- cumulative_sales(sales, OrderDate)
  monthly_cumulative    <- cumulative_sales(sales, Year, Month)
  quarterly_cumulative  <- cumulative_sales(sales, Year, Quarter)

  # Combine into a single summary table
  cumulative_summary <- bind_rows(
    daily_cumulative      %>% mutate(metric_type = "Daily"),
    monthly_cumulative    %>% mutate(metric_type = "Monthly"),
    quarterly_cumulative  %>% mutate(metric_type = "Quarterly")
  )

  # Store in the date_tables list
  date_tables[["cumulative_summary"]] <- cumulative_summary

  # ----------------------------
  # Step 7: sales growth
  # ----------------------------


  # Function to calculate sales growth (percent change) over any time period
  sales_growth <- function(data, ...) {
    sales_data <- data %>%
      group_by(...) %>%   #group by one or more columns
      summarise(
        period_sales = sum(Price, na.rm = TRUE),    #total sales in each period
        .groups = "drop"    #ungroup after summarise
      ) %>%
      arrange(...) %>%   #order by the same columns
      mutate(
        previous_sales = lag(period_sales),    #    sales from previous period
        percent_change  = (period_sales - previous_sales) / previous_sales * 100
      )

    return(sales_data)    #return the calculated table
  }

  # Calculate growth for different time periods
  daily_growth      <- sales_growth(sales, OrderDate) #daily sales growth
  monthly_growth    <- sales_growth(sales, Year, Month) #monthly sales growth
  quarterly_growth  <- sales_growth(sales, Year, Quarter) #quarterly sales growth

  # Combine all growth tables into a single summary table
  growth_summary <- bind_rows(
    daily_growth      %>% mutate(metric_type = "Daily"), #label for daily growth
    monthly_growth    %>% mutate(metric_type = "Monthly"), #label for monthly growth
    quarterly_growth  %>% mutate(metric_type = "Quarterly") #label for quarterly growth
  )

  # Store the summary table in the date_tables list
  date_tables[["growth_summary"]] <- growth_summary #store combined table in list



  # ----------------------------
  # end step: Return all processed tables
  # ----------------------------
  return(date_tables)
}