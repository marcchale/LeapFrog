#' @title Plot coordinates with tour
#' @description Takes a coordinate matrix and plots a tour through it
#' @export
#' @param coordMatrix A \code{n}x\code{2 or 3} data.frame where first row is not column headers. 
#' The first and second columns must contain the \code{x} and \code{y} coordinates of each node.
#' The third column (optional) must contain ids for each node. 
#' @param tour A \code{numeric} vector
#' @param latlong A \code{boolean} value. \code{TRUE} if coordinates are latitudes and longitudes, \code{FALSE} otherwise.
#' @examples 
#' put example here
#' 

plot_tour <- function(coordMatrix, tour, latlong = FALSE){

  df <- data.frame(x = coordMatrix[,1],
                   y = coordMatrix[,2],
                   ID = coordMatrix[,3])
  df <- df[tour,]
  df <- rbind(df, df[1,])
  yOffset <- (range(coordMatrix[,2])[2] - range(coordMatrix[,2])[1]) * 0.025
  
  suppressWarnings(ggplot2::theme_set(ggplot2::theme_void()))
  ggplot2::ggplot(data = df,
                  ggplot2::aes(x = x,
                               y = y)) +
    ggplot2::geom_point() + 
    ggplot2::geom_path() + 
    ggplot2::coord_fixed() +
    ggplot2::geom_text(ggplot2::aes(y = y + yOffset,
                                    label = ID))

}



