## @knitr ex_b_set
offline <- readData("offline.final.trace.txt")
# fix up the xypos
offline$posXY <-  paste(offline$posX, offline$posY, sep = "-")
byLocAngleAP = with(offline, 
                    by(offline, list(posXY, angle, mac), 
                       function(x) x))

signalSummary = 
  lapply(byLocAngleAP,            
         function(oneLoc) {
           ans = oneLoc[1, ]
           ans$medSignal = median(oneLoc$signal)
           ans$avgSignal = mean(oneLoc$signal)
           ans$num = length(oneLoc$signal)
           ans$sdSignal = sd(oneLoc$signal)
           ans$iqrSignal = IQR(oneLoc$signal)
           ans
           })
offlineSummary_original = do.call("rbind", signalSummary)     
offlineSummary <- offlineSummary_original %>% filter(mac != router_b)

online <-  readData("online.final.trace.txt", subMacs = unique(offlineSummary$mac))
online$posXY <-  paste(online$posX, online$posY, sep = "-")

keepVars = c("posXY", "posX","posY", "orientation", "angle")
byLoc = with(online, 
             by(online, list(posXY), 
                function(x) {
                  ans = x[1, keepVars]
                  avgSS = tapply(x$signal, x$mac, mean)
                  y = matrix(avgSS, nrow = 1, ncol = 6,
                        dimnames = list(ans$posXY, names(avgSS)))
                  cbind(ans, y)
                }))

onlineSummary = do.call("rbind", byLoc)

## @knitr calcError

## @knitr funs1

reshapeSS1 <-  function(data, varSignal = "signal", 
                        keepVars = c("posXY", "posX","posY")) {
  byLocation <- with(data, by(data, list(posXY), 
                              function(x) {
                                ans <-  x[1, keepVars]
                                avgSS <-  tapply(x[ , varSignal ], x$mac, mean)
                                y <-  matrix(avgSS, nrow = 1, ncol = 6,
                                             dimnames = list(ans$posXY,
                                                             names(avgSS)))
                                cbind(ans, y)
                              }))

  newDataSS <- do.call("rbind", byLocation)
  return(newDataSS)
}


selectTrain1 <-  function(angleNewObs, signals = NULL, m = 1){
  # m is the number of angles to keep between 1 and 5
  refs <-  seq(0, by = 45, length  = 8)
  nearestAngle <-  roundOrientation(angleNewObs)

  if (m %% 2 == 1) 
    angles <-  seq(-45 * (m - 1) /2, 45 * (m - 1) /2, length = m)
  else {
    m = m + 1
    angles <-  seq(-45 * (m - 1) /2, 45 * (m - 1) /2, length = m)
    if (sign(angleNewObs - nearestAngle) > -1) 
      angles <-  angles[ -1 ]
    else 
      angles <-  angles[ -m ]
  }
  # round angles
  angles <-  angles + nearestAngle
  angles[angles < 0] <-  angles[ angles < 0 ] + 360
  angles[angles > 360] <-  angles[ angles > 360 ] - 360
  angles <-  sort(angles) 

  offlineSubset <-  signals[ signals$angle %in% angles, ]
  reshapeSS1(offlineSubset, varSignal = "avgSignal")
}


findNN1 <-  function(newSignal, trainSubset) {
  diffs <-  apply(trainSubset[ , 4:9], 1, 
                  function(x) x - newSignal)
  dists <-  apply(diffs, 2, function(x) sqrt(sum(x^2)) )
  closest <-  order(dists)
  weightDF <-  trainSubset[closest, 1:3 ]
  weightDF$weight <-  1/closest
  return(weightDF)
}


predXY1 <-  function(newSignals, newAngles, trainData, 
                    numAngles = 1, k = 3){
  closeXY <-  list(length = nrow(newSignals))

  for (i in 1:nrow(newSignals)) {
    trainSS <-  selectTrain1(newAngles[i], trainData, m = numAngles)
    closeXY[[i]] <- findNN1(newSignal = as.numeric(newSignals[i, ]), trainSS)
  }

  estXY <- lapply(closeXY, 
                 function(x) sapply(x[ , 2:3], 
                                    function(x) mean(x[1:k])))
  estXY <- do.call("rbind", estXY)
  return(estXY)
}

## @knitr summaries

byLocAngleAP = with(offline, 
                    by(offline, list(posXY, angle, mac), 
                       function(x) x))

signalSummary = 
  lapply(byLocAngleAP,            
         function(oneLoc) {
           ans = oneLoc[1, ]
           ans$medSignal = median(oneLoc$signal)
           ans$avgSignal = mean(oneLoc$signal)
           ans$num = length(oneLoc$signal)
           ans$sdSignal = sd(oneLoc$signal)
           ans$iqrSignal = IQR(oneLoc$signal)
           ans
           })

offlineSummary_original = do.call("rbind", signalSummary)     
offlineSummary <- offlineSummary_original %>% filter(mac != router_b)


keepVars = c("posXY", "posX","posY", "orientation", "angle")
byLoc = with(online, 
             by(online, list(posXY), 
                function(x) {
                  ans = x[1, keepVars]
                  avgSS = tapply(x$signal, x$mac, mean)
                  y = matrix(avgSS, nrow = 1, ncol = 6,
                        dimnames = list(ans$posXY, names(avgSS)))
                  cbind(ans, y)
                }))

onlineSummary = do.call("rbind", byLoc)


## @knitr knn_0_0

estXYk3 = predXY1(newSignals = onlineSummary[ , 6:11], 
                 newAngles = onlineSummary[ , 4], 
                 offlineSummary, numAngles = 3, k = NNeighbors)

# nearest neighbor
estXYk1 = predXY1(newSignals = onlineSummary[ , 6:11], 
                 newAngles = onlineSummary[ , 4], 
                 offlineSummary, numAngles = 3, k = 1)

## @knitr grid_setup_0

v = 11 
permuteLocs = sample(unique(offlineSummary$posXY))
permuteLocs = matrix(permuteLocs, ncol = v, 
                     nrow = floor(length(permuteLocs)/v))

onlineFold = subset(offlineSummary, posXY %in% permuteLocs[ , 1])
reshapeSS1 = function(data, varSignal = "signal", 
                     keepVars = c("posXY", "posX","posY"),
                     sampleAngle = FALSE, 
                     refs = seq(0, 315, by = 45)) {
  byLocation =
    with(data, by(data, list(posXY), 
                  function(x) {
                    if (sampleAngle) {
                      x = x[x$angle == sample(refs, size = 1), ]}
                    ans = x[1, keepVars]
                    avgSS = tapply(x[ , varSignal ], x$mac, mean)
                    y = matrix(avgSS, nrow = 1, ncol = 6,
                               dimnames = list(ans$posXY,
                                               names(avgSS)))
                    cbind(ans, y)
                  }))

  newDataSS = do.call("rbind", byLocation)
  return(newDataSS)
}


offline = offline[ offline$mac != "00:0f:a3:39:dd:cd", ]

keepVars = c("posXY", "posX","posY", "orientation", "angle")

onlineCVSummary = reshapeSS1(offline, keepVars = keepVars, 
                            sampleAngle = TRUE)

onlineFold = subset(onlineCVSummary, 
                    posXY %in% permuteLocs[ , 1])

offlineFold = subset(offlineSummary,
                     posXY %in% permuteLocs[ , -1])

estFold = predXY1(newSignals = onlineFold[ , 6:11], 
                 newAngles = onlineFold[ , 4], 
                 offlineFold, numAngles = 1, k = 3)

actualFold = onlineFold[ , c("posX", "posY")]

## @knitr grid_0_0

NNeighbors = 20
K = NNeighbors 
err = numeric(K)

for (j in 1:v) {
  onlineFold = subset(onlineCVSummary, 
                      posXY %in% permuteLocs[ , j])
  offlineFold = subset(offlineSummary,
                       posXY %in% permuteLocs[ , -j])
  actualFold = onlineFold[ , c("posX", "posY")]
  
  for (k in 1:K) {
    estFold = predXY1(newSignals = onlineFold[ , 6:11],
                     newAngles = onlineFold[ , 4], 
                     offlineFold, numAngles = 1, k = k)
    err[k] = err[k] + calcError(estFold, actualFold)
  }
}
