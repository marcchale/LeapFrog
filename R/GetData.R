#' @title Distance Data for the Travelling Salesman Problem
#' @description Outputs a list containing the name, distance matrix, coordinate matrix, and known optimal value from a TSPLIB data instance
#' @export
#' @param course A \code{string} description of the desired instance information.
#' 

GetData <- function(course = c("eil51", "ts225", "pr1002", "gr120", "rat195", 
                             "Bays29", "Berlin52", "Cho130", "KroA100", 
                             "pcb442", "pr76", "gr48", "pma343"))
{
  # Error handling
  if(course %in% c("eil51", "ts225", "pr1002", "gr120", "rat195", 
                   "Bays29", "Berlin52", "Cho130", "KroA100", 
                   "pcb442", "pr76", "gr48", "pma343"))
  {}else{
    stop("Unknown Parameter: course must be one of the following:
          eil51, ts225, pr1002, gr120, rat195, Bays29, Berlin52, Cho130, KroA100, pcb442, pr76, gr48, pma343")
  }
  strDist <- base::paste0(course, "_dist")
  strCoord <- base::paste0(course, "_coord")
  strOptimal <- base::paste0(course, "_optimal")
  data(list = strDist)
  data(list = strCoord)
  data(list = strOptimal)
  courseData <- base::list(course = course, 
                           distanceMatrix = get(strDist), 
                           coordinateMatrix = get(strCoord), 
                           optimalValue = get(strOptimal))
  rm(list = strDist, envir = .GlobalEnv)
  rm(list = strCoord, envir = .GlobalEnv)
  rm(list = strOptimal, envir = .GlobalEnv)
  return(courseData)
}
