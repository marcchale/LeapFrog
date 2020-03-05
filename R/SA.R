#' @title Simulated Annealing Comparison
#' @description Runs the Simulated Annealing Algorithm for the same time duration that Leap Frog required
#' @param LFObj A LeapFrog class object 
#' @param temp The starting temperature
#' @param alpha The annealing rate
#' 
SACompare <- function(LFObj, temp = 200, alpha = 0.9){
  tol <- LFObj$time # Time out limit (stop at same time as LeapFrog)
  
  tour <- base::sample(base::seq_len(LFObj$nodeCount)) # Create a random starting tour
  distBest <- tourDist <- TourLength(LFObj$distances, tour)
  timeStart <- base::proc.time() # Start a timer
  iterDataSA <- base::matrix(c(0, tourDist), ncol = 2)
  base::colnames(iterDataSA) <- c("Time", "Length")
  
  while((base::proc.time()-timeStart)[3] < tol){ 
    tourTest <- tour # Clone tour and manipulate it's clone
    nodeSwap <- base::sort(base::sample(LFObj$nodeCount, 2))
    tourTest <- base::replace(tourTest, nodeSwap[1]:nodeSwap[2], base::rev(tourTest[nodeSwap[1]:nodeSwap[2]])); 
    distTest <- TourLength(LFObj$distances, tourTest)
    
    # If candidate tour is worse then current tour create the probability value we will accept it at, if it is better accept it
    if (distTest > tourDist){ 
      ratio <- base::exp((tourDist - distTest) / temp) # If candidate tour is worse then current tour create the probability value we will accept it anyway
    } else { 
      ratio <- 1 # Logical test, if candidate tour < current tour then ratio = 1 (i.e probability acceptance = 1)  
    } 
    
    # Draw uniform random number, and accept candidate with probability as calculated above
    if (stats::runif(1) < ratio){
      tour <- tourTest
      tourDist <- distTest
    } 
    
    # Check to see if current tour is better than best-so-far
    if (tourDist < distBest) { # If new tour is better then best tour seen so far 
      tourBest <- tour           # update best so far tour order and,
      distBest <- tourDist      # update best so far cost
    } 
    # Reduce the temperature by the decay rate
    temp <- temp * alpha
    iterDataSA <- rbind(iterDataSA, 
                        c((base::proc.time()-timeStart)[3], distBest))
  } 
  iterDataSA <- as.data.frame(iterDataSA)
  lims <- ggplot2::ggplot_build(LFObj$lfHist)$layout$panel_scales_y[[1]]$range$range
  h <- LFObj$lfHist +
    ggplot2::geom_point(data = iterDataSA,
                        ggplot2::aes(x = Time,
                                     y = Length),
                        color = "green")
  if(base::max(iterDataSA$Length) > lims[2]){
    h <- h +
    ggplot2::ylim(lims[1], base::max(iterDataSA$Length) + 20)
    }
  print(h)
  LFObj$lfHist <- h
  return(LFObj)
}