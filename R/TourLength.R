#' @title Calculate tour length
#' @description Takes a tour and distance matrix and returns the tour length
#' @param distances A \code{n}x\code{n matrix} where first row is not column headers.
#' Each cell represents the distance from the row index node to the column index node.
#' @param tour A \code{numeric} vector with the order that the nodes in the coordMatrix are visited. Must be the same length as the coordMatrix
#' 

TourLength <- function(distances, tour){
  #check to see if distances is a matrix
  if(!is.matrix(distances)){
    stop("Invalid Parameter: distances must be a matrix")
  }
  #check to see if coordMatrix has correct number of columns
  if(dim(distances)[1] != dim(distances)[2]){
    stop("Invalid Input: distances must be a square matrix")
  }
  #check that all entries in distances are numeric
  if(!all(apply(distances, c(1,2), function(x) is.numeric(x)))){
    stop("Invalid Paramter: distances may only contain numeric values")
  }
  #check if distances has at least three entries
  if(dim(distances)[1] < 3){
    stop("Trivial Problem Size: distances should contain at least three nodes")
  }
  #check if tour is a  vector
  if(!is.vector(tour)){
    stop("Invalid Parameter: tour must be a vector")
  }
  #check that all entries in tour are numeric
  if(!all(sapply(tour, function(x) is.numeric(x)))){
    stop("Invalid Paramter: tour may only contain numeric values")
  }
  #check that tour is correct length
  if(length(tour) != dim(distances)[1]){
    stop("Invalid Input: tour is not correct length")
  }
  nodeCount <- length(tour)
  temp.dist <- 0 # Reset distance stored in memory
  for (k in 1:(nodeCount-1)){
    temp.dist <- sum(distances[tour[k],tour[k+1]], # Distance from first to second node
                     temp.dist) # Previous sum of distances
  } 
  temp.dist <- sum(distances[tour[nodeCount],tour[1]], # Distance from first to second node
                   temp.dist) # Previous sum of distances
  return(temp.dist)
}
