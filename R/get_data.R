#' @title Distance Data for the Travelling Salesman Problem
#' @description Outputs a list containing the distance matrix and coordinate matrix from a TSPLIB data instance
#' @export
#' @param map A \code{string} description of the desired instance information.
#' @examples 
#' put example here

get_data <- function(course = c("eil51", "ts225", "pr1002", "gr120", "rat195", 
                             "Bays29", "Berlin52", "Cho130", "KroA100", 
                             "pcb442", "pr76", "gr48", "pma343"))
{
  strDist <- base::paste0(course, "_dist")
  strCoords <- base::paste0(course, "_coord")
  strOptimal <- base::paste0(course, "_optimal")
  courseDist <- data(list = strDist)
  courseCoords <- data(list = strCoords)
  courseOptimal <- data(list = strOptimal)
  courseData <- base::list(course, courseDist, courseCoords, courseOptimal)
  return(courseData)
}
