#' @title Distance Data for the Travelling Salesman Problem
#' @description Outputs a list containing the name, distance matrix, coordinate matrix, and known optimal value from a TSPLIB data instance
#' @export
#' @param course A \code{string} description of the desired instance information.
#' 

GetData <- function(instance = c("eil51", "ts225", "pr1002", "gr120", "rat195", 
                             "Bays29", "Berlin52", "Cho130", "KroA100", 
                             "pcb442", "pr76", "gr48", "pma343"))
{
  # Error handling
  if(instance %in% c("eil51", "ts225", "pr1002", "gr120", "rat195", 
                   "Bays29", "Berlin52", "Cho130", "KroA100", 
                   "pcb442", "pr76", "gr48", "pma343", 1:13))
  {}else{
    stop("Unknown Parameter: course must be one of the following:
          eil51, ts225, pr1002, gr120, rat195, Bays29, Berlin52, Cho130, KroA100, pcb442, pr76, gr48, pma343, or an integer from 1 to 13")
  }
  data(list = "dataList")
  courseData <- dataList[[instance]]
  rm(list = "dataList", envir = .GlobalEnv)
  return(courseData)
}
