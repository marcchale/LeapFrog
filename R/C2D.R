#' @title Convert Coordinate Data.Frame to Distance Matrix
#' @description Convert coordinate Matrix into symmetric distance matrix using euclidean distance function
#' @param coordMatrix A \code{n}x\code{2 or 3 data.frame} where first row is not column headers. 
#' The first and second columns must contain the \code{x} and \code{y} coordinates of each node.
#' The third column (optional) must contain ids for each node. 
#' 

C2D <- function(coordinates, nodeCount){
  
  
  #get Node IDs from coordMatrix with 3 columns
  if(ncol(coordinates) == 3){
    IDs <- vector(mode = "character", length = nodeCount)
    for(id in seq_len(nodeCount)){
      IDs[id] <- as.character(coordinates$ID[id])  
    }
  }
  distances <- matrix(0, nrow = nodeCount, ncol = nodeCount)
  colnames(distances) <- IDs
  rownames(distances) <- IDs
  for(i in seq_len(nodeCount)){
    for(j in seq_len(nodeCount)){
      dx <- as.numeric(coordinates[i,1])-as.numeric(coordinates[j,1])
      dy <- as.numeric(coordinates[i,2])-as.numeric(coordinates[j,2])
      distances[i,j] <- sqrt(dx^2 + dy^2)
    }
  }
  return(distances)
}