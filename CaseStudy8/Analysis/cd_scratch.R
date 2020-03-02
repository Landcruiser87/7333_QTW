

# load packages -----------------------------------------------------------
library(quantmod)
library(tswge)
library(dplyr)
library(magrittr)
library(tidyverse)
library(forecast)

# Data Functions ----------------------------------------------------------



getStonk <- function(ticker) {
  fromDate = '2018-02-01'
  toData = '2020-02-01'
  data <- getSymbols(ticker, auto.assign = F, from= fromDate, to=toData, env = NULL)
  headers = c('open', 'high', 'low', 'close', 'volume', 'adjusted')
  data = xts(data)
  names(data) = headers
  test = data[(nrow(data)-4):nrow(data),]
  data = data[1:(nrow(data)-5),]
  resp = list(
    ticker = ticker,
    data=data,
    test=test
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




# Fit Model ---------------------------------------------------------------
modelStonk <- function(stonk){
  stonk$est = est.arma.wge(stonk$diff,p = stonk$aic5[[1]][[1]], q = stonk$aic5[[2]][[1]])
  stonk$mod = fore.aruma.wge(stonk$data$close, 
                             phi = stonk$est$phi,
                             d = 1,
                             theta = stonk$est$theta,
                             n.ahead = 5, 
                             lastn = F)
  stonk$lbox = ljung.wge(stonk$est$res, 
                         p = stonk$aic5[[1]][[1]], 
                         q = stonk$aic5[[2]][[1]]
                         )
  return(stonk)
}


# Test Functions ----------------------------------------------------------



stonks = lapply(c('CROX'), getStonk)

# plot stonks -------------------------------------------------------------

# lapply(stonks, plotBasic)

stonks = lapply(stonks, getDiff)

lapply(stonks, function(x) auto.arima(x$data$close, ic = "bic", num.cores = NULL))
stonks = lapply(stonks, modelStonk)
stonk = stonks[[1]]