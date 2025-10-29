
library(dplyr)

library(R6)

data_filler <- function(dataset) {

  # Ensure columns exist
  if(!"Quantity" %in% names(dataset)) dataset$Quantity <- NA
  if(!"TotalPrice" %in% names(dataset)) dataset$TotalPrice <- NA

  sales <- dataset %>%
    mutate(
      PricePU = ifelse(
        !is.na(Price) & !is.na(Quantity),
        Price / Quantity,
        NA
      )
    )

  # Default prices per item
  default_prices <- sales %>%
    group_by(Item) %>%
    summarise(
      DefaultPricePU = mean(PricePU, na.rm = TRUE),
      .groups = "drop"
    )

  # Create R6 objects
  price_objects <- list()
  defVal <- R6Class(
    "defVal",
    private = list(Item = NULL, PricePU = NULL),
    public = list(
      initialize = function(item = NA, PricePU = NA) {
        private$Item <- item
        private$PricePU <- PricePU
      },
      getValues = function() list(Item = private$Item, PricePU = private$PricePU)
    )
  )

  for (i in 1:nrow(default_prices)) {
    obj <- defVal$new(
      item = default_prices$Item[i],
      PricePU = default_prices$DefaultPricePU[i]
    )
    price_objects[[default_prices$Item[i]]] <- obj
  }

  # Fill missing Quantity or TotalPrice
  for (i in 1:nrow(sales)) {
    item_name <- sales$Item[i]
    obj <- price_objects[[item_name]]

    # Only try if columns exist
    if(!is.null(sales$Quantity[i]) && !is.null(sales$TotalPrice[i])) {
      if(is.na(sales$Quantity[i]) & !is.na(sales$TotalPrice[i])) {
        sales$Quantity[i] <- sales$TotalPrice[i] / obj$getValues()$PricePU
      }

      if(is.na(sales$TotalPrice[i]) & !is.na(sales$Quantity[i])) {
        sales$TotalPrice[i] <- sales$Quantity[i] * obj$getValues()$PricePU
      }
    }
  }

  return(sales)
}
