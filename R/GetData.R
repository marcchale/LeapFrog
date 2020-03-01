#' @title Distance Data for the Travelling Salesman Problem
#' @description Outputs a list containing the name, distance matrix, coordinate matrix, and known optimal value from a TSPLIB data instance
#' @export
#' @param instance A \code{string} description of the desired instance information. The courses are listed in order: eil51, ts225, pr1002, gr120, rat195,Bays29, Berlin52, Cho130, KroA100, pcb442, pr76, gr48, pma343
#' 

GetData <- function(instance = 1)
{
  # Error handling
  if(instance %in% 1:13){
  }else{
    stop("Unknown Parameter: instance must be an integer from 1 to 13")
  }
  utils::data(list = "dataList")
  courseData <- dataList[[instance]]
  base::rm(list = "dataList", envir = .GlobalEnv)
  return(courseData)
}
