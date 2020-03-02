

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
  data <- getSymbols(ticker, 
                     auto.assign = F, 
                     from= fromDate, 
                     to=toData, 
                     env = NULL)
  
  ## Rename Columns to be stonk agnostic
  headers = c('open', 'high', 'low', 'close', 'volume', 'adjusted')
  data = xts(data)
  names(data) = headers
  
  ## Split last 5 into test to use for later
  ## Remove last 5 from main data
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
  ## Get diff, just doing dif=1 for now
  stonk$diff = artrans.wge(stonk$data$close, 1)
  # plotts.sample.wge(stonk$diff)
  
  ## Get top 5 pq combos using "bic"
  stonk$aic5 = aic5.wge(stonk$diff,p=0:8, q=0:5,type = 'bic')
  # Get acf data for diff data
  stonk$acf = acf(stonk$diff)
  return(stonk)
}

#Taking the first difference of the crox data.




# Fit Model ---------------------------------------------------------------
modelStonk <- function(stonk){
  # Estimate phi and theta model using best p q values from aic5 and diff
  stonk$est = est.arma.wge(stonk$diff,p = stonk$aic5[1,1], q = stonk$aic5[1,2])
  
  # Fit arima model with estimated phis and thetas from est
  stonk$mod = fore.aruma.wge(stonk$data$close, 
                             phi = stonk$est$phi,
                             d = 1,
                             theta = stonk$est$theta,
                             n.ahead = 5, 
                             lastn = F)
  
  # Test residuals of estimated model residuals (might need to be resid from mod)
  stonk$lbox = ljung.wge(stonk$est$res, 
                         p = stonk$aic5[1,1], 
                         q = stonk$aic5[1,2]
                         )
  
  # Lastly calculate ASE from test data and forcasted values
  stonk$ASE =  mean((stonk$test$close - stonk$mod$f)^2)
  return(stonk)
}


# Test Functions ----------------------------------------------------------



stonks = lapply(c('CROX', 'AMD','MSFT'), getStonk)

# plot stonks -------------------------------------------------------------

# lapply(stonks, plotBasic)

stonks = lapply(stonks, getDiff)
stonks = lapply(stonks, modelStonk)

lapply(stonks, function(x) auto.arima(x$data$close, ic = "bic", num.cores = NULL))