## @knitr processline
# first we define the processline function, 
# which unsurprisingly processes a single line of the offline or online.txt
library(tidyverse)
processLine = function(x) {
  # here we split the line at the weird markers. Strsplit returns a list
  # we take the first item of the list
  tokens = strsplit(x, "[;=,]")[[1]]
  if (length(tokens) == 10){
    return(NULL)
  }
  # now we are going to stack the tokens
  tmp = matrix(tokens[ - (1:10) ], , 4, byrow = TRUE)
  cbind(matrix(tokens[c(2, 4, 6:8, 10)], nrow(tmp), 6,byrow = TRUE),
  tmp)
}

## @knitr roundO
roundOrientation = function(angles) {
  refs = seq(0, by = 45, length  = 9)
  q = sapply(angles, function(o) which.min(abs(o - refs)))
  c(refs[1:8], 0)[q]
}

## @knitr readData
# this reads in the data
readData <- function(filename, 
                     subMacs = c("00:0f:a3:39:e1:c0", "00:0f:a3:39:dd:cd", "00:14:bf:b1:97:8a",
                                 "00:14:bf:3b:c7:c6", "00:14:bf:b1:97:90", "00:14:bf:b1:97:8d",
                                 "00:14:bf:b1:97:81")) {
  # read it in line by line
  txt = readLines(filename)
  # ignore comments
  lines = txt[ substr(txt, 1, 1) != "#" ]
  # process (tokenize and stack) each line
  tmp = lapply(lines, processLine)
  # rbind each elemnt of the list together
  offline = as.data.frame(do.call(rbind, tmp), 
                          stringsAsFactors= FALSE) 
  # set the names of our matrix
  names(offline) = c("time", "scanMac", 
                     "posX", "posY", "posZ", "orientation", 
                     "mac", "signal", "channel", "type")

  # keep only signals from access points
  offline = offline[ offline$type == "3", ]

  # drop scanMac, posZ, channel, and type - no info in them
  dropVars = c("scanMac", "posZ", "channel", "type")
  offline = offline[ , !( names(offline) %in% dropVars ) ]

  # drop more unwanted access points
  offline = offline[ offline$mac %in% subMacs, ]

  # convert numeric values
  numVars = c("time", "posX", "posY", "orientation", "signal")
  offline[numVars] = lapply(offline[numVars], as.numeric)

  # convert time to POSIX
  offline$rawTime = offline$time
  offline$time = offline$time/1000
  class(offline$time) = c("POSIXt", "POSIXct")

  # round orientations to nearest 45
  offline$angle = roundOrientation(offline$orientation)

  return(offline)
}

## @knitr offline_read
offline <- readData("offline.final.trace.txt")
