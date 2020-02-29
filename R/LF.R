#' @title LeapFrog metaheuristic search algorithm
#' @description Takes a distance matrix and searches for an optimal tour
#' 
#' @export
#' @param LFObj A LeapFrog class object created by running the ImportData() function.
#' @param p A \code{double} (0,1] which represents the ratio of the maximum number of nodes removed in each iteration of the LF algorithm.
#' @param m An \code{integer} (0,inf) which represents the number of games played
#' @param s A \code{double} (0,1] which represents the uncertainty used in the first round of each game
#' @param r An \code{integer} (0,inf) which represents the number of rounds in each game
#' @param a A \code{double} (0,1] which represents the variable number of players used in successive rounds
#' @param monitor A \code{boolean} or function used to display information during execution of the algorithm
#' 

# p = players (0,1], r = rounds (0,inf), s = accuracy (0,1], m = games (0,inf), a = decayRate (0,1]

LF <- function(LFObj, 
               p = 1,
               m = 1, 
               s = 10^-3, 
               r = 1, 
               a = 0, 
               monitor = TRUE){
  # Tests
  nodeCount <- LFObj$nodeCount
  paramTest(nodeCount, p, m, s, r, a, monitor)
  m <- as.integer(m)
  r <- as.integer(r)
  a <- a * r # Set a as a percentage of r
  loss <- FALSE
  if (a > 0) loss <- TRUE
  
  # create initial random tour and get tour length
  distances <- LFObj$distances
  coordinates <- LFObj$coordinates
  tour <- tourBest <- LFObj$tour
  tourLength <- tourLengthBest <- LFObj$tourLength
  iterData <- matrix(NA, ncol = 4)
  colnames(iterData) <- c("Match", "Round", "Length", "Best")
  
  # begin algorithm
  gameIter <- 0
  gameCount <- 0
  
  while(TRUE){
    # Update game counters
    gameIter <- gameIter + 1
    
    # Set pPrime
    if(gameIter == 1){
      pPrime <- p * (nodeCount - 4)
    } else {
      if (loss) pPrime <- pPrime - (p * (nodeCount - 4) /  (r * a))
    }
    
    # Jump
    jumpers <- sample(1:nodeCount, size = ceiling(pPrime))
    tour <- tour[!(tour %in% jumpers)]
    
    # Land
    for(node in 1:ceiling(pPrime)){
      landScores <- LandDist(distances, ceiling(pPrime), nodeCount, tour, jumpers, node)
      if(gameIter == 1){
        placeSize <- round((nodeCount - ((nodeCount - 4) * p)) * s) #Place with selected accuracy
      } else {
        placeSize <- 1 # Place in best location
      }
      if (placeSize < 1) placeSize <- 1
      tourPlaceRand <- sample(1:placeSize)[1]
      tourPlace <- order(landScores, decreasing = FALSE)[tourPlaceRand] # Pick the landing spot
      
      if(tourPlace < length(tour)){
        tour <- c(tour[1:tourPlace], jumpers[node], tour[(tourPlace + 1):length(tour)])
      } else {
        tour <- c(tour, jumpers[node]) # Place the jumper in the chosen landing spot
      }
    }
    
    # Recalculate tour length
    tourLength <- TourLength(distances, tour)
    if (tourLength < tourLengthBest){
      tourBest <- tour
      tourLengthBest <- tourLength
    }
    iterData <- rbind(iterData, c(gameCount + 1, gameIter, tourLength, tourLengthBest))
    # Check game progress
    if (gameIter == r){
      gameIter <- 0 # Start a new game
      gameCount <- gameCount + 1 # increment game counter
      # Show summary
      if(monitor == T) lfMonitor(gameCount, tourLengthBest)
    }
    if (gameCount == m){
      iterData <- as.data.frame(na.omit(iterData))
      if(LFObj$knownOpt != 0){
        goal <- LFObj$knownOpt
      } else {
        goal <- NA
      }
      LFObj$tour <- tourBest
      LFObj$tourLength <- tourLengthBest
      LFObj$p <- PlotTour(LFObj)
      LFObj$lfHist <- ggplot2::ggplot(data = iterData,
                                  ggplot2::aes(x = seq_len(m*r),#Round,
                                               y = Length)) +
        ggplot2::geom_point() + 
        ggplot2::geom_hline(yintercept = tourLengthBest,
                            color = "red",
                            linetype = "dashed") +
        theme(
          legend.position = c(.95, .95),
          legend.justification = c("right", "top"),
          legend.box.just = "right",
          legend.margin = margin(6, 6, 6, 6)
          ) +
        # Title, axes, citation
        ggplot2::labs(title = "LeapFrog Algorithm Iteration History",
                      y = "Tour Length",
                      x = "Iteration",
                      caption = "Source: LeapFrog") +
        ggplot2::theme_bw() +
        annotate(geom="text", x=50, y=LFObj$tourLength, label="Best found",
                 color="red")
      if(!is.na(goal)){
        LFObj$lfHist <- LFObj$lfHist + annotate(geom="text", x=25, y=goal, label="Best known",
                                                color="blue") +
          ggplot2::geom_hline(yintercept = goal,
                              color = "blue")
      }
      break
    }
  }
  return(LFObj)
}
