#' @title Plot coordinates with tour
#' @description Takes a coordinate matrix and plots a tour through it
#' @param coordDF A \code{n}x\code{2 or 3} data.frame or tibble where first row is not column headers. 
#' The first and second columns must contain the \code{x} and \code{y} coordinates of each node.
#' The third column (optional) must contain ids for each node. 
#' @param tour A \code{numeric} vector with the order that the nodes in the coordDF are visited. Must be the same length as the coordDF
#' @param latlong A \code{boolean} value. \code{TRUE} if coordinates are latitudes and longitudes, \code{FALSE} otherwise.
#' 

PlotTour <- function(LFObj){
  df <- LFObj$coordinates[LFObj$tour,]
  df <- rbind(df, df[1,])
  yOffset <- (range(df$Y)[2] - range(df$Y)[1]) * 0.025
  
  suppressWarnings(ggplot2::theme_set(ggplot2::theme_void()))
  p <- ggplot2::ggplot(data = df,
                       ggplot2::aes(x = X,
                                    y = Y)) +
    ggplot2::geom_point() + 
    ggplot2::geom_path() + 
    ggplot2::coord_fixed() +
    ggplot2::geom_text(ggplot2::aes(y = Y + yOffset,
                                    label = ID))
  return(p)
}



