

# load packages -----------------------------------------------------------
library(quantmod)
library(tswge)
library(dplyr)
library(magrittr)
library(tidyverse)
library(forecast)

# Data Functions ----------------------------------------------------------



getStonk <- function(ticker) {
  fromDate = '2019-01-02'
  toData = '2020-01-02'
  data <- getSymbols(ticker, auto.assign = F, from= fromDate, to=toData, env = NULL)
  headers = c('open', 'high', 'low', 'close', 'volume', 'adjusted')
  data = xts(data)
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



stonks = lapply(c('CROX'), getStonk)

# plot stonks -------------------------------------------------------------

lapply(stonks, plotBasic)

stonks = lapply(stonks, getDiff)

lapply(stonks, function(x) auto.arima(x$data$close, ic = "bic", num.cores = NULL))

