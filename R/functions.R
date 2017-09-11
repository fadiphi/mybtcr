#' Retrieve wallet information
#'
#' @param none
#' @keywords wallet bct
get.wallet <- function (market="kraken",currency.pair=c("BTC","EUR"),file="~/.kraken_keys.txt"){
    key <- scan(file=file,what=list(api="",private=""))
    market.api.process(market, currency.pair ,action = "wallet", key = key$api, secret = key$private)
}

#' Retrieve ticker information
#'
#' It only displays current highest bid and lowest ask
#' 
#' @param none
#' @keywords ticker bct
get.ticker <- function(market="kraken",currency.pair=c("BTC","EUR")){market.api.process(market, currency.pair, "ticker")}

#' Retrieve trades information
#'
#' It only displays last trades
#' 
#' @param none
#' @keywords trades bct
get.trades <- function(market="kraken",currency.pair=c("BTC","EUR")){market.api.process(market,currency.pair,"trades")}

#' Retrieve order book informations
#'
#' It only displays order book info
#' 
#' @param none
#' @keywords trades bct
get.order.book <- function(market="kraken",currency.pair=c("BTC","EUR")){market.api.process(market,currency.pair,"order_book")}

#' Plot an order book 
#'
#' Plots an orderbook, default the last one available
#' 
#' @param ob the orderbook
#' @keywords trades bct
plot.order.book <- function(ob=get.order.book()){Rbitcoin.plot(ob)}

#' Retrieve bids info from the last order book 
#'
#' It only displays order book info
#' 
#' @param nn=10 number of bids to show
#' @keywords trades bct
get.bids <- function(nn=10,market="kraken",currency.pair=c("BTC","EUR")){head(get.order.book()$bids,n=nn)}

#' Retrieve asks info from the last order book 
#'
#' It only displays order book info
#' 
#' @param nn=10 number of asks to show
#' @keywords trades bct
get.asks <- function(nn=10,market="kraken",currency.pair=c("BTC","EUR")){head(get.order.book()$asks,n=nn)}

#' Buy an amount of btc at a given price
#'
#' @param amount Number of btc needed; min \code{0.01}
#' @param price Price per btc,  max precision \code{10^-1}
buy <- function(amount, price,market="kraken",currency.pair=c("BTC","EUR"),file="~/.kraken_keys.txt"){
    key <- scan(file=file,what=list(api="",private="")) 
    bought <- market.api.process(market,
                                 currency.pair,
                                 action ="place_limit_order",
                                 req = list(type = "buy",
                                            price = price,
                                            amount = amount),
                                 key = key$api,
                                 secret = key$private)
    return(bought)}
#' Sell an amount of btc at a given price
#'
#' @param amount Number of btc needed; min \code{0.01}
#' @param price Price per btc,  max precision \code{10^-1}
sell <- function(amount, price,market="kraken",currency.pair=c("BTC","EUR"),file="~/.kraken_keys.txt"){
    key <- scan(file=file,
                what=list(api="",private="")) 
    sold <- market.api.process(market,
                       currency.pair,
                       action ="place_limit_order",
                       req = list(type = "sell",
                                  price = price,
                                  amount = amount),
                       key = key$api, secret = key$private)
    return(sold)}

#' Get your open orders
open.orders <- function(market="kraken",currency.pair=c("BTC","EUR"),file="~/.kraken_keys.txt"){
    key <- scan(file=file,what=list(api="",private="")) 
    orders <- market.api.process(market,
                       currency.pair,
                       action = "open_orders",
                       key = key$api,
                       secret = key$private)
    return(orders)}

#'Cancel an order
#' @param c The OID of the order. you can retreive it with \code{open.orders}
cancel.order <- function(c,market="kraken",currency.pair=c("BTC","EUR"),file="~/.kraken_keys.txt") {
        key <- scan(file=file,what=list(api="",private="")) 
        cancelled <- market.api.process(market,
                                        currency.pair,action = "cancel_order",
                                        req = list(oid = c),
                                        key = key$api,
                                        secret = key$private)
        return(cancelled)}

#' Give a situation by a data.frame of [open,high,low,close] every t minutes since id
#'
#' @param t Number of minutes between open and close. Possible numbers are 1 (default), 5, 15, 30, 60, 240 (4h), 1440 (1D), 10080 (1W), 21600 (15D). Default \code{t=1}
#' @param id The time in second from "1970-01-01" since you need the values. Default \code{id=0} in order to have the maximum amount possible of data
#' @return A data frame with columns [time,open,high,low,close,vol]
get.history <- function(t=1,id=0,market="kraken",currency.pair=c("BTC","EUR"),){
## for reference https://www.kraken.com/help/api
## 1. time in second from 0000UTC
## 2. open
## 3. high
## 4. low
## 5. close
## 6. wwap
## 7. volume
## 8. count

## available time frame interval in minutes
## 1 (default), 5, 15, 30, 60, 240 (4h), 1440 (1D), 10080 (1W), 21600 (15D)



    time.interval <- t
    url <- paste('https://api.kraken.com/0/public/OHLC?pair=XBTEUR&interval=',time.interval,'&since=',id,sep='')

    ## gives a list, reference on how to deal with it http://www.r-tutor.com/r-introduction/list
    history <- market.api.query(market,url)

    ## not the best way to access a list, but it's working

    start <- 1
    for (i in start:length(history$result$XXBTZEUR)){
        if (i==start){
            open <- as.numeric(history$result$XXBTZEUR[[i]][[2]])
            high <- as.numeric(history$result$XXBTZEUR[[i]][[3]])
            low <- as.numeric(history$result$XXBTZEUR[[i]][[4]])
            close <- as.numeric(history$result$XXBTZEUR[[i]][[5]])

            vol <- as.numeric(history$result$XXBTZEUR[[i]][[7]])
            time.raw <- as.numeric(history$result$XXBTZEUR[[i]][[1]])
        }
        else{
            open <- c(open,as.numeric(history$result$XXBTZEUR[[i]][[2]]))
            high <- c(high,as.numeric(history$result$XXBTZEUR[[i]][[3]]))
            low <- c(low,as.numeric(history$result$XXBTZEUR[[i]][[4]]))
            close <- c(close,as.numeric(history$result$XXBTZEUR[[i]][[5]]))

            vol <- c(vol,as.numeric(history$result$XXBTZEUR[[i]][[7]]))
            time.raw <- c(time.raw,as.numeric(history$result$XXBTZEUR[[i]][[1]]))
        }
    }

    ## create a xts to be used for quantmod
    time <- as.POSIXct(time.raw,origin="1970-01-01")#,tz='UTC') #to make a time stamp from seconds
    return(data.frame(time,open,high,low,close,vol))
##    candleChart(vol.series)
    
  ##  addVo()

}
