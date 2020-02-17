#' @title Convert Coordinate Data.Frame to Distance Matrix
#' @description Convert coordinate Matrix into symmetric distance matrix using euclidean distance function
#' @export
#' @param coordMatrix A \code{n}x\code{2 or 3 data.frame} where first row is not column headers. 
#' The first and second columns must contain the \code{x} and \code{y} coordinates of each node.
#' The third column (optional) must contain ids for each node. 
#' @examples 
#' put example here
#' 

C2D <- function(coordMatrix){
  #check to see if coordMatrix is a data.frame
  if(!is.data.frame(coordMatrix)){
    stop("Invalid Parameter: coordMatrix must be a data frame")
  }
  #check to see if coordMatrix has correct number of columns
  if(dim(coordMatrix)[2] < 2 | dim(coordMatrix)[2] > 3){
    stop("Invalid Input: coordMatrix must contain 2-3 columns")
  }
  #check that all entries in first two columns are numeric
  if(!all(apply(coordMatrix[,1:2], c(1,2), function(x) is.numeric(x)))){
    stop("Invalid Paramter: the first two columns of coordMatrix must contain all numeric inputs")
  }
  #check if coordMatrix has at least three entries
  if(dim(coordMatrix)[1] < 3){
    stop("Trivial Problem Size: coordMatrix should contain at least three nodes")
  }
  #check if coordMatrix has only two columns
  if(dim(coordMatrix)[2] == 2){
    message("Node IDs not specified. ID's will be automatically assigned.")
    IDs <- base::paste0("Node_", 1:dim(coordMatrix)[1])
  }
  #get Node IDs from coordMatrix with 3 columns
  if(dim(coordMatrix)[2] == 3){
    IDs <- vector(mode = "character", length = dim(coordMatrix)[1])
    for(id in 1:dim(coordMatrix)[1]){
      IDs[id] <- as.character(coordMatrix[id,3])  
    }
  }
  distMatrix <- matrix(0, nrow = dim(coordMatrix)[1], ncol = dim(coordMatrix)[1])
  colnames(distMatrix) <- IDs
  rownames(distMatrix) <- IDs
  for(i in 1:dim(coordMatrix)[1]){
    for(j in 1:dim(coordMatrix)[1]){
      dx <- as.numeric(coordMatrix[i,1])-as.numeric(coordMatrix[j,1])
      dy <- as.numeric(coordMatrix[i,2])-as.numeric(coordMatrix[j,2])
      distMatrix[i,j] <- sqrt(dx^2 + dy^2)
    }
  }
  return(distMatrix)
}