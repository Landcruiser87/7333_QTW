---
title: "Slater is bad at R please change this title"
author: "David Josephs, Andy Heroy, Carson Drake, Che' Cobb"
date: "2020-01-16"
output: 
  html_document:
    toc: true
    number_sections: true
    theme: united
    highlight: haddock
    df_print: paged
    keep_md: TRUE
    fig_width: 12
    fig_height: 12
    fig_retina: true
---














# Things to do



<div class="figure" style="text-align: center">
<img src="d_draft0_files/figure-html/firstplot-1.svg" alt="Signal vs Orientation at Nolan and Lang's Selected MAC"  />
<p class="caption">Signal vs Orientation at Nolan and Lang's Selected MAC</p>
</div>

Here is the MAC in question

<div class="figure" style="text-align: center">
<img src="d_draft0_files/figure-html/secondplot-1.svg" alt="Signal vs Orientation of the much hated bad mac"  />
<p class="caption">Signal vs Orientation of the much hated bad mac</p>
</div>

Comment on differences fix captions, this mac looks DAMN GOOD TO ME


Recreate that stupid plot, and do it for the other MAC, then improve


<div class="figure" style="text-align: center">
<img src="d_draft0_files/figure-html/dens0-1.svg" alt="dumb"  />
<p class="caption">dumb</p>
</div>

<img src="d_draft0_files/figure-html/hated00-1.svg" style="display: block; margin: auto;" />

<div class="figure" style="text-align: center">
<img src="d_draft0_files/figure-html/dens1-1.svg" alt="dumb"  />
<p class="caption">dumb</p>
</div>

<div class="figure" style="text-align: center">
<img src="d_draft0_files/figure-html/hated01-1.svg" alt="dumb"  />
<p class="caption">dumb</p>
</div>


# code stops at ipynb chunk 39

Figure out fig sizes pls

# Appendix




```r
options( digits = 2)
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
roundOrientation = function(angles) {
  refs = seq(0, by = 45, length  = 9)
  q = sapply(angles, function(o) which.min(abs(o - refs)))
  c(refs[1:8], 0)[q]
}
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

offline_original <- readData("../Data/offline.final.trace.txt")
bad_mac <-  "00:0f:a3:39:dd:cd"
good_mac <- "00:0f:a3:39:e1:c0"
fixfonts <- theme(text = element_text(family = "serif", , face = "bold"))
plt_theme <- ggthemes::theme_wsj()  + fixfonts

offline_original %>% mutate(angle = factor(angle)) %>%
  filter(posX == 2 & posY == 12 & mac != bad_mac) %>%
  ggplot() + geom_boxplot(aes(y = signal, x= angle)) + 
  facet_wrap(. ~ mac, ncol = 2) +  
  ggtitle("Signal vs Orientation at 6 of 7 MACs") + plt_theme
offline_original %>% mutate(angle = factor(angle)) %>%
  filter(posX == 2 & posY == 12 & mac == bad_mac) %>%
  ggplot() + geom_boxplot(aes(y = signal, x= angle)) + 
  facet_wrap(. ~ mac)  + 
  ggtitle("Signal vs Orientation at the missing MAC") + plt_theme
offline_original %>% mutate(angle = factor(angle)) %>%
  filter(posX == 2 & posY == 12 & mac != bad_mac) %>%
  ggplot(aes(signal)) + geom_density()+
  facet_wrap(mac ~ angle) +  
  ggtitle("Signal Density at each angle of each Mac Except the One we hate") + plt_theme + ggthemes::scale_fill_hc()
offline_original %>% mutate(angle = factor(angle)) %>%
  filter(posX == 2 & posY == 12 & mac == bad_mac) %>%
  ggplot(aes(signal)) + geom_density()+
  facet_wrap(mac ~ angle) +  
  ggtitle("Signal Density Per angle at the hated MAC") + plt_theme + ggthemes::scale_fill_hc()
offline_original %>% mutate(angle = factor(angle)) %>%
  filter(posX == 2 & posY == 12 & mac != bad_mac) %>%
  ggplot(aes(signal, fill = angle )) + geom_density()+
  facet_wrap(. ~ mac, ncol = 2) +  
  ggtitle("Signal Density per MAC, ") + plt_theme + scale_fill_viridis_d()
offline_original %>% mutate(angle = factor(angle)) %>%
  filter(posX == 2 & posY == 12 & mac == bad_mac) %>%
  ggplot(aes(signal, fill = angle )) + geom_density()+
  facet_wrap(. ~ mac) +
  ggtitle("Signal density per angle at the hated mac") + plt_theme + scale_fill_viridis_d()
```

