#' @title Plot coordinates with tour
#' @description Takes a coordinate matrix and plots a tour through it
#' @export
#' @param coordMatrix A \code{n}x\code{2 or 3} data.frame where first row is not column headers. 
#' The first and second columns must contain the \code{x} and \code{y} coordinates of each node.
#' The third column (optional) must contain ids for each node. 
#' @param tour A \code{numeric} vector with the order that the nodes in the coordMatrix are visited. Must be the same length as the coordMatrix
#' @param latlong A \code{boolean} value. \code{TRUE} if coordinates are latitudes and longitudes, \code{FALSE} otherwise.
#' @examples 
#' put example here
#' 

plot_tour <- function(coordMatrix, tour, latlong = FALSE){
  #check to see if coordMatrix is a data.frame
  if(!is.data.frame(coordMatrix)){
    stop("Invalid Input: coordMatrix must be a data frame")
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
  #check if tour is a vector
  if(!is.vector(tour)){
    stop("Invalid Input: tour must be a numeric vector")
  }
  #check if every element of tour is numeric
  if(!all(sapply(tour,function(x) is.numeric(x)))){
    stop("Invalid Parameter: each element of tour must be numeric")
  }
  #check if every element of tour is numeric
  if(length(tour) != dim(coordMatrix)[1]){
    stop("Invalid Input: tour must contain exactly as many nodes as coordMatrix")
  }
  #check if every element of tour is unique
  if(length(unique(tour)) != dim(coordMatrix)[1]){
    stop("Invalid Input: each element of tour must be unique")
  }
  #check if coordMatrix has only two columns
  if(dim(coordMatrix)[2] == 2){
    message("Node IDs not specified. ID's will be automatically assigned.")
    coordMatrix$ID <- base::paste0("Node_", 1:dim(coordMatrix)[1])
  }else{
    #get Node IDs from coordMatrix with 3 columns
    IDs <- vector(mode = "character", length = dim(coordMatrix)[1])
    for(id in 1:dim(coordMatrix)[1]){
      IDs[id] <- as.character(coordMatrix[id,3])  
    }
    coordMatrix$ID <- IDs
  }
  
  
  
  df <- data.frame(X = coordMatrix[,1],
                   Y = coordMatrix[,2],
                   ID = coordMatrix$ID)
  df <- df[tour,]
  df <- rbind(df, df[1,])
  yOffset <- (range(coordMatrix[,2])[2] - range(coordMatrix[,2])[1]) * 0.025
  
  suppressWarnings(ggplot2::theme_set(ggplot2::theme_void()))
  p <- ggplot2::ggplot(data = df,
                       ggplot2::aes(x = X,
                                    y = Y)) +
    ggplot2::geom_point() + 
    ggplot2::geom_path() + 
    ggplot2::coord_fixed() +
    ggplot2::geom_text(ggplot2::aes(y = Y + yOffset,
                                    label = ID))
  print(p)

}



