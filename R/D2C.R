#' @title distMatrix to coordDF
#' @description Takes a distance matrix and converts it into a coordinate tibble
#' 
#' @export
#' @param distMatrix A \code{n}x\code{n matrix} where first row is not column headers.
#' Each cell represents the distance from the row index node to the column index node.
#' 
D2C <- function(distMatrix){
  nodeCount <- dim(distMatrix)[1]
  coordDF <- tibble::tibble(X = c(0,distMatrix[1,2]),
                            Y = c(0,0))
  for (i in 3:nodeCount){
  x <- (distMatrix[i-1, i]^2 + distMatrix[i-2, i]^2) / (-2 * distMatrix[i-2, i-1])
  y <- (i / 2) + sqrt(abs(distMatrix[i-1, i] ^2 - x^2))
  coordDF[i,] <- c(x + distMatrix[i-2, i-1], y)
 
  }
  return(coordDF)
}
