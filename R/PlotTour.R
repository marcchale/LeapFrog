#' @title Plot coordinates with tour
#' @description Takes a coordinate matrix and plots a tour through it
#' @export
#' @param coordDF A \code{n}x\code{2 or 3} data.frame or tibble where first row is not column headers. 
#' The first and second columns must contain the \code{x} and \code{y} coordinates of each node.
#' The third column (optional) must contain ids for each node. 
#' @param tour A \code{numeric} vector with the order that the nodes in the coordDF are visited. Must be the same length as the coordDF
#' @param latlong A \code{boolean} value. \code{TRUE} if coordinates are latitudes and longitudes, \code{FALSE} otherwise.
#' @examples 
#' put example here
#' 

PlotTour <- function(coordDF, tour, latlong = FALSE){
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
  
  
  
  df <- data.frame(X = coordDF[,1],
                   Y = coordDF[,2],
                   ID = coordDF$ID)
  df <- df[tour,]
  df <- rbind(df, df[1,])
  yOffset <- (range(coordDF[,2])[2] - range(coordDF[,2])[1]) * 0.025
  
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



