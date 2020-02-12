#' @title Calculates opportunity costs to insert a node
#' @description Takes a tour and distance matrix and returns the tour length
#' 
#' @param distMatrix A \code{n}x\code{n matrix} where first row is not column headers.
#' Each cell represents the distance from the row index node to the column index node.
#' @param tour A \code{numeric} vector with the order that the nodes in the coordMatrix are visited. Must be the same length as the coordMatrix
#' @examples 
#' put example here
#' 

land_dist <- function(){
  temp.place <- rep(0, (node.count-ceiling(p)-1+i)) # Create an empty vector to store the differences
  for (j in 1:(node.count-ceiling(p)-2+i)){
    temp.place[j] <- sum(distance.matrix[jumpers[i],tour[j]], # New arc 2
                         distance.matrix[jumpers[i],tour[j+1]], # New arc 3
                         -distance.matrix[tour[j],tour[j+1]]) # Deleted arc 3
  }
  temp.place[(node.count-ceiling(p)-1+i)] <- sum(distance.matrix[jumpers[i],tour[node.count-1-ceiling(p)+i]], # New arc 2
                                                 distance.matrix[jumpers[i],tour[1]], # New arc 3
                                                 -distance.matrix[tour[node.count-1-ceiling(p)+i],tour[1]]) # Deleted arc 3
  return(temp.place)
}