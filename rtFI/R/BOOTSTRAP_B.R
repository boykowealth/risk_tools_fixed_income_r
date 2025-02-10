#' Bootstrap Yield Curve from Bond Prices and Coupons
#'
#' This function bootstraps a zero-coupon yield curve from bond prices and coupon rates.
#' It assumes annual coupons and that the bond prices are known.
#'
#' @param data A data frame with bond prices, coupon rates, and maturities.
#' @return A tibble with maturities and zero rates.
#' @export
#' @examples
#' bond_data <- tibble(maturity = c(1, 2, 3, 4),
#'                     price = c(98, 97, 95, 92),
#'                     coupon = c(0.05, 0.05, 0.05, 0.05))
#' BOOTSTRAP_B(data = bond_data)

BOOTSTRAP_B <- function(data) {
  
  n <- nrow(data)
  zero_rates <- numeric(n)
  
  for (i in 1:n) {
    maturity <- data$maturity[i]
    price <- data$price[i]
    coupon <- data$coupon[i]
    
    if (i == 1) {
      discount_factors[i] <- 1 / (1 + coupon)
    } else {
      sum_df <- 0
      for (j in 1:(i-1)) {
        sum_df <- sum_df + coupon * discount_factors[j]
      }
      # The discount factor for the current bond
      discount_factors[i] <- (coupon * sum_df + 1) / price
    }
    
    # Calculate the zero rates from the discount factors
    zero_rates[i] <- sqrt(1 / discount_factors[i]) - 1
  }
  
  result <- dplyr::tibble(
    maturity = data$maturity,
    zero_rate = zero_rates
  )
  
  return(result)
}

bond_data <- tibble(maturity = c(1, 2, 3, 4),
                    price = c(98, 97, 95, 92),
                    coupon = c(0.05, 0.05, 0.05, 0.05))
BOOTSTRAP_B(data = bond_data)

