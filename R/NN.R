#' @title Nearest Neighbor algorithm
#' @description Runs the nearest neighbor algorithm starting from each node, then returns shortest tour
#' @param LFObj A LeapFrog class object
#' 
NN <- function(LFObj){
  nnTime <- base::proc.time()
  base::colnames(LFObj$distances) <- base::seq_len(LFObj$nodeCount)
  lengths <- base::vector()
  NNBestTour <- NULL
  NNBestLength <- NULL
  
  NNInternal <- function(tour){
    for(node in 2:LFObj$nodeCount){
      tour <- c(tour, base::as.integer(base::names(base::which.min(LFObj$distances[tour[node-1], -tour]))))
    }
    return(tour)
  }
  
  for(tour in base::seq_len(LFObj$nodeCount)){
    tourIter <- NNInternal(tour)
    lengths[tour] <- TourLength(LFObj$distances, tourIter)
    if(lengths[tour] == base::min(lengths)){
      NNBestTour <- tourIter
      NNBestLength <- lengths[tour]
    }
  }
  nnTime <- (base::proc.time() - nnTime)[3]
  return(base::list(tour = NNBestTour, length = NNBestLength, time = nnTime))
}


#' @title Nearest Neighbor comparison tool
#' @description Runs the nearest neighbor function and adds the results to the lfHist plot
#' @param LFObj A LeapFrog class object
#' 

NNCompare <- function(LFObj){
  #iterData <- NULL
  nn <- NN(LFObj)
  h <- LFObj$lfHist %+% 
    ggplot2::geom_text(x = nn$time,
                       y = nn$length,
                       label = "NN",
                       color = "orange")
  lims <- ggplot2::ggplot_build(LFObj$lfHist)$layout$panel_scales_y[[1]]$range$range
  if(nn$length > lims[2]){
    
    h <- h +
      ggplot2::ylim(lims[1], nn$length + 20)
  }
  print(base::paste0("Best NN tour length = ", nn$length))
  LFObj$lfHist <- h
  print(LFObj$lfHist)
  return(LFObj)
}