MySummaryR <- function(vecIn){
  n <- length(vecIn)
  sumVec <- 0
  
  for (i in vecIn) sumVec = sumVec + i
  myMean <- sumVec / n

  sampVar <- 0
  for (i in vecIn) sampVar <- sampVar + (i - myMean) ** 2
  adjVar <- (1 / (n - 1)) * sampVar
  
  out <- list()
  
  out$mean = myMean
  out$std = sqrt(adjVar)
  
  return(out)
}


MySummaryR2 <- function(vecIn){
  n <- length(vecIn)
  
  myMean <- sum(vecIn) / n
  
  adjVar <- 1 / (n - 1) * sum((vecIn - myMean) ** 2)
  
  out <- list()
  
  out$mean = myMean
  out$std = sqrt(adjVar)
  
  return(out)
}
