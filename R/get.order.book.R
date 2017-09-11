#' Retrieve order book informations
#'
#' It only displays order book info
#' 
#' @param none
#' @keywords trades bct
get.order.book <- function(){market.api.process(market,currency.pair,"order_book")}
