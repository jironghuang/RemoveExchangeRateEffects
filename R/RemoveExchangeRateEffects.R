#' RemoveExchangeRateEffects: A package for decomposing exchange rates
#'
#' The RemoveExchangeRateEffects package allows you dto ecompose 1 instrument position in SGD (column value) - from the perspective of someone staying in Singapore - into local static value (i.e if I keep the exchange rate constant at the start of the period) and the residual exchange rate impact.
#'
#' @section exchange_rate_decomposition class:
#' The class aids you in stripping out exchange effects (e.g. investment portfolio, revenue)
#' 
#' @section multiple_exchange_rate_decomposition class:
#' Works like previous class. But you can use this on multiple instruments at once
#'
#' @docType package
#' @name RemoveExchangeRateEffects
NULL
