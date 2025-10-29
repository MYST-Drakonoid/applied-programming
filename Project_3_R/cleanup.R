

library(jsonlite)
library(dplyr)
library(stringr)


clean_data <- function(input_path) {

  # import data
  data <- fromJSON(input_path)

  # ----------------------------
  # Convert columns only if they exist
  # ----------------------------
  if("OrderDate" %in% names(data)) data$OrderDate <- as.Date(data$OrderDate)
  if("Amount" %in% names(data))    data$Amount    <- as.numeric(data$Amount)
  if("Price" %in% names(data))     data$Price     <- as.numeric(data$Price)

  # Fix missing values
  if("Customer" %in% names(data)) data$Customer[is.na(data$Customer)] <- "Unknown"
  if("Comments" %in% names(data)) data$Comments[is.na(data$Comments)] <- "NoComment"

  # Clean text
  if("Item" %in% names(data)) {
    data$Item <- str_trim(str_replace_all(data$Item, "[[:punct:]]", ""))
    data$Item <- str_to_title(data$Item)  # unified capitalization
  }

  if("Comments" %in% names(data)) {
    data$Comments <- str_to_sentence(data$Comments)
  }

  # Remove duplicate rows
  data <- distinct(data)

  return(data)
}