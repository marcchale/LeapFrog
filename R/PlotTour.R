#' @title Plot coordinates with tour
#' @description Takes a coordinate matrix and plots a tour through it
#' @param LFObj A \code{LeapFrog} class object
#' 

PlotTour <- function(LFObj){
  X <- Y <- ID <- NULL
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



