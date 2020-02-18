#' @title Calculate tour length
#' @description Takes a tour and distance matrix and returns the tour length
#' @export
#' @param distMatrix A \code{n}x\code{n matrix} where first row is not column headers.
#' Each cell represents the distance from the row index node to the column index node.
#' @param tour A \code{numeric} vector with the order that the nodes in the coordMatrix are visited. Must be the same length as the coordMatrix
#' 

TourLength <- function(distMatrix, tour){
  #check to see if distMatrix is a matrix
  if(!is.matrix(distMatrix)){
    stop("Invalid Parameter: distMatrix must be a matrix")
  }
  #check to see if coordMatrix has correct number of columns
  if(dim(distMatrix)[1] != dim(distMatrix)[2]){
    stop("Invalid Input: distMatrix must be a square matrix")
  }
  #check that all entries in distMatrix are numeric
  if(!all(apply(distMatrix, c(1,2), function(x) is.numeric(x)))){
    stop("Invalid Paramter: distMatrix may only contain numeric values")
  }
  #check if distMatrix has at least three entries
  if(dim(distMatrix)[1] < 3){
    stop("Trivial Problem Size: distMatrix should contain at least three nodes")
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
  if(length(tour) != dim(distMatrix)[1]){
    stop("Invalid Input: tour is not correct length")
  }
  node.count <- length(tour)
  temp.dist <- 0 # Reset distance stored in memory
  for (k in 1:(node.count-1)){
    temp.dist <- sum(distMatrix[tour[k],tour[k+1]], # Distance from first to second node
                     temp.dist) # Previous sum of distances
  } 
  temp.dist <- sum(distMatrix[tour[node.count],tour[1]], # Distance from first to second node
                   temp.dist) # Previous sum of distances
  return(temp.dist)
}
