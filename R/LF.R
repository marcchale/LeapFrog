#' @title LeapFrog metaheuristic search algorithm
#' @description Takes a distance matrix and searches for an optimal tour
#' 
#' @export
#' @param distMatrix A \code{n}x\code{n matrix} where first row is not column headers.
#' Each cell represents the distance from the row index node to the column index node.
#' @param coordDF A \code{n}x\code{2 or 3} data.frame or tibble where first row is not column headers. 
#' The first and second columns must contain the \code{x} and \code{y} coordinates of each node.
#' The third column (optional) must contain ids for each node. 
#' @param p A \code{double} (0,1] which represents the ratio of the maximum number of nodes removed in each iteration of the LF algorithm.
#' @param m An \code{integer} (0,inf) which represents the number of games played
#' @param s A \code{double} (0,1] which represents the uncertainty used in the first round of each game
#' @param r An \code{integer} (0,inf) which represents the number of rounds in each game
#' @param a A \code{double} (0,1] which represents the variable number of players used in successive rounds
#' @param monitor A \code{boolean} or function used to display information during execution of the algorithm
#' @examples 
#' put example here
#' 

# p = players (0,1], r = rounds (0,inf), s = accuracy (0,1], m = games (0,inf), a = decayRate (0,1]

LF <- function(distMatrix, 
              coordDF, 
               p=1,
               m=1, 
               s=10^-3, 
               r=1, 
               a=0, 
               monitor = if(interactive()) lfMonitor else FALSE){
  # Tests
  if(!is.matrix(distMatrix)) stop("Invalid Input: distMatrix must be a matrix")
  if(dim(distMatrix)[1] != dim(distMatrix)[2]) stop("Invalid Input: distMatrix must be a square matrix")
  if(any(is.na(distMatrix))) stop("Invalid Input: distMatrix must not contain NAs")
  if(!all(apply(distMatrix, c(1,2), function(x) is.numeric(x)))) stop("Invalid Input: distMatrix must contain all numeric inputs")
  nodeCount <- dim(distMatrix)[1]
  if(nodeCount < 5) stop("Invalid Input: problem is too small, distMatrix must be at least 5x5")
  
  if(length(p) != 1) stop("Invalid Input: p must be a single integer")
  if(!is.numeric(p) | is.na(p) | p <= 0 | p > 1) stop("Invalid Input: p must be a single value (0,1]")
  
  if(length(s) != 1) stop("Invalid Input: s must be a single value")
  if(!is.numeric(s) | is.na(s) | s <= 0 | s > 1) stop("Invalid Input: s must be a single value (0,1]")
  
  if(length(m) != 1) stop("Invalid Input: m must be a single integer")
  if(!is.numeric(m) | is.na(m) | m <= 0) stop("Invalid Input: m must be an integer greater than 0")
  m <- as.integer(m)
  
  if(length(r) != 1) stop("Invalid Input: r must be a single integer")
  if(!is.numeric(r) | is.na(r) | r <= 0) stop("Invalid Input: r must be an integer greater than 0")
  r <- as.integer(r)
  
  if(length(a) != 1) stop("Invalid Input: a must be a single value")
  if(!is.numeric(a) | is.na(a) | a < 0 | a > 1) stop("Invalid Input: a must be a single value [0,1]")
  a <- a * r # Set a as a percentage of r
  loss <- FALSE
  if (a > 0) loss <- TRUE
  
  if(!missing(coordDF)){
    #check to see if coordDF is a data.frame
    if(!is.data.frame(coordDF)) stop("Invalid Input: coordDF must be a data frame")
    
    #check to see if coordDF has correct number of columns
    if(dim(coordDF)[2] < 2 | dim(coordDF)[2] > 3) stop("Invalid Input: coordDF must contain 2-3 columns")
    
    #check that all entries in first two columns are numeric
    if(!all(apply(coordDF[,1:2], c(1,2), function(x) is.numeric(x)))) stop("Invalid Paramter: the first two columns of coordDF must contain all numeric inputs")
    
    #check if coordDF has at least three entries
    if(dim(coordDF)[1] < 3) stop("Trivial Problem Size: coordDF should contain at least three nodes")
    
    #check if tour is a vector
    if(!is.vector(tour)) stop("Invalid Input: tour must be a numeric vector")
    
    #check if every element of tour is numeric
    if(!all(sapply(tour,function(x) is.numeric(x)))) stop("Invalid Parameter: each element of tour must be numeric")
    
    #check if every element of tour is numeric
    if(length(tour) != dim(coordDF)[1]) stop("Invalid Input: tour must contain exactly as many nodes as coordDF")
    
    #check if every element of tour is unique
    if(length(unique(tour)) != dim(coordDF)[1]) stop("Invalid Input: each element of tour must be unique")
    
    #check if coordDF has only two columns
    if(dim(coordDF)[2] == 2){
      message("Note: Node IDs not specified. ID's will be automatically assigned.")
      coordDF$ID <- base::paste0("Node_", 1:dim(coordDF)[1])
    }else{
      #get Node IDs from coordDF with 3 columns
      IDs <- vector(mode = "character", length = dim(coordDF)[1])
      for(id in 1:dim(coordDF)[1]){
        IDs[id] <- as.character(coordDF[id,3])  
      }
      coordDF$ID <- IDs
    }
  }
  
  if(!(monitor %in% c(TRUE, FALSE, plot))) stop("Invalid Input: monitor must be TRUE, FALSE, or plot")
  
  # create initial random tour and get tour length
  tour <- tourBest <- sample(1:nodeCount)
  tourLength <- tourLengthBest <- TourLength(distMatrix, tour)
  
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
      landScores <- LandDist(distMatrix, ceiling(pPrime), nodeCount, tour, jumpers, node)
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
    tourLength <- TourLength(distMatrix, tour)
    if (tourLength < tourLengthBest){
      tourBest <- tour
      tourLengthBest <- tourLength
    }
    
    # Check game progress
    if (gameIter == r){
      gameIter <- 0 # Start a new game
      gameCount <- gameCount + 1 # increment game counter
      # Show summary
      if(monitor == T) lfMonitor(gameCount, tourLengthBest)
    }
    if (gameCount == m) break
    
    
  }
  #setMethod("plot", "lf", plot.lf)
  return(list(distance = tourLengthBest,
              solution = tourBest))
}
