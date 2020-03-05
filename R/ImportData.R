#' @title Converts raw inputs into an LF class object
#' @description Takes a distance, coordinate, and tour information and creates an LF class object.
#' @export
#' @param distances A \code{n}x\code{n matrix} where first row is not column headers.
#' @param coordinates A \code{n}x\code{2 or 3} data object where first row is not column headers. Not required, but node names may be included as the first column.
#' @param tour An \code{integer vector} containing the order to visit each node in a solution.
#' @param optimal A \code{numeric} containing the solution tour length.
#' @param knownOpt A \code{numeric} containing the known optimal tour length.
#' @param latlon A \code{boolean} TRUE if coordinates are latitudes and longitudes, 0 otherwise.
#' 

ImportData <- function(distances = NULL,
                       coordinates = NULL, 
                       tour = NULL,
                       optimal = NA,
                       knownOpt = NA,
                       latlon = FALSE){
  dTrack <- F
  cTrack <- F
  if(!missing(distances)){
    if(is.null(ncol(distances))) stop("Invalid Input: distances must have equal numbers of rows and columns")
    if(nrow(distances) != ncol(distances)) stop("Invalid Input: distances must have equal numbers of rows and columns")
    if(!all(apply(distances, c(1,2), is.numeric))) stop("Invalid Input: all entries in distances must be numeric")
    if(any(apply(distances, c(1,2), is.na))) stop("Invalid Input: distances cannot contain NAs")
    
    dTrack <- T
  }
  
  if(!missing(coordinates)){
    if(is.null(ncol(coordinates))) stop("Invalid Input: coordinates must contain two or three columns")
    if(ncol(coordinates) < 2 | ncol(coordinates) > 3) stop("Invalid Input: coordinates must contain two or three columns")
    startCol <- 1
    if(ncol(coordinates) == 3) startCol <- 2
    if(!all(apply(coordinates[,startCol:(startCol + 1)], c(1,2), is.numeric))) stop("Invalid Input: coordinate columns must be numeric")
    if(any(apply(coordinates, c(1,2), is.na))) stop("Invalid Input: coordinates cannot contain NAs")
    if(nrow(coordinates) < 3) stop("Problem too small, you need at least three nodes")
    
    cTrack <- T
  }
  
  if(dTrack && cTrack && nrow(distances) != nrow(coordinates)) stop("distances and coordinates do not contain same number of nodes")
  if(!dTrack & !cTrack) stop("distances or coordinates or both must be included in ImportData()")
  
  if(dTrack & !cTrack){
    distances <- as.matrix(distances)
    coordinates <- D2C(distances)
    }
  
  coordinates <- as.data.frame(coordinates)
  colnames(coordinates)[1:2] <- c("X", "Y")
  nodeCount <- nrow(coordinates)
  if(ncol(coordinates) == 2){
    message("Note: Node IDs not specified. ID's will be automatically assigned.")
    coordinates$ID <- paste0("Node_", seq_len(nodeCount))
  }
  
  if(!dTrack & cTrack){ 
    distances <- C2D(coordinates, nodeCount)
    }
  
  if(!missing(tour)){
    if(!is.null(ncol(tour)) && ncol(tour) != 1 && nrow(tour) != 1) stop("tour must be one dimensional")
    if(length(tour) != nodeCount) stop("tour must be nonrepeating vector containing the numbers from 1 to the number of nodes")
  } else {
    tour <- sample(seq_len(nodeCount))
  }
  
  if(dim(distances)[1] < 5) message("LF algorithm requires problem instance to contain at least five nodes")    
  
  out.list <- list(distances = distances,
                   coordinates = coordinates,
                   tour = tour,
                   nodeCount = nodeCount,
                   p = NULL,
                   tourLength = TourLength(distances, tour),
                   knownOpt = knownOpt,
                   latlon = latlon,
                   lfHist = NULL,
                   time = NULL)
  class(out.list) <- c("LFObj")
  out.list$p <- PlotTour(out.list)
  
  print(out.list$p)
  return(out.list)
}


