#' @title Calculates opportunity costs to insert a node
#' @description Takes a tour and distance matrix and returns the tour length
#' 
#' @param dMat A \code{n}x\code{n matrix} where first row is not column headers.
#' Each cell represents the distance from the row index node to the column index node.
#' @param tour A \code{numeric} vector with the order that the nodes in the coordMatrix are visited. Must be the same length as the coordMatrix
#' @param p A \code{numeric} that tells the function how many jumpers are being considered
#' @param nodeCount The \code{numeric} number of nodes in the problem instance
#' @param jumpers An \code{integer vector} of the jumpers currently in the air
#' @param node The \code{integer} specific jumper currently under consideration

LandDist <- function(dMat, p, nodeCount, tour, jumpers, node){
  tempPlace <- rep(0, (nodeCount-ceiling(p)-1+node)) # Create an empty vector to store the differences
  for (j in 1:(nodeCount-ceiling(p)-2+node)){
    tempPlace[j] <- sum(dMat[jumpers[node],tour[j]], # New arc 2
                         dMat[jumpers[node],tour[j+1]], # New arc 3
                         -dMat[tour[j],tour[j+1]]) # Deleted arc 3
  }
  tempPlace[(nodeCount-ceiling(p)-1+node)] <- sum(dMat[jumpers[node],tour[nodeCount-1-ceiling(p)+node]], # New arc 2
                                                 dMat[jumpers[node],tour[1]], # New arc 3
                                                 -dMat[tour[nodeCount-1-ceiling(p)+node],tour[1]]) # Deleted arc 3
  return(tempPlace)
}


