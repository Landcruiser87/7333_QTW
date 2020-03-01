

# load packages -----------------------------------------------------------
library(quantmod)
library(tswge)
library(dplyr)
library(magrittr)
library(tidyverse)


# Data Functions ----------------------------------------------------------



getStonk <- function(ticker) {
  data <- getSymbols(ticker, auto.assign = F, )
  headers = c('open', 'high', 'low', 'close', 'volume', 'adjusted')
  data = data.frame(xts(data))
  names(data) = headers
  resp = list(
    ticker = ticker,
    data=data
  )
  return(resp)
}



# Plot Functions ----------------------------------------------------------



plotCandle <- function(stonk){
  return(candleChart(stonk$data, theme = 'white', name = stonk$ticker))
}

plotBasic <- function(stonk){
  plotts.wge(stonk$data$close)
  plotts.sample.wge(stonk$data$close)
  
}



# Diffs -------------------------------------------------------------------

getDiff <- function(stonk){
  stonk$diff = artrans.wge(stonk$data$close, 1)
  plotts.sample.wge(stonk$diff)
  stonk$aic5 = aic5.wge(stonk$diff,type = 'bic')
  stonk$acf = acf(stonk$diff)
  return(stonk)
}

#Taking the first difference of the crox data.



# Test Functions ----------------------------------------------------------



stonks = lapply(c('AMD','AAPL','NVDA'), getStonk)

# plot stonks -------------------------------------------------------------

lapply(stonks, plotBasic)

stonks = lapply(stonks, getDiff)