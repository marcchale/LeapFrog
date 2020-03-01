paramTest <- function(nodeCount = 1,
                      p = 1,
                      m = 1, 
                      s = 10^-3, 
                      r = 1, 
                      a = 0, 
                      monitor = TRUE){
  if(nodeCount < 5) stop("Invalid Input: problem is too small, distMatrix must be at least 5x5")
  
  if(length(p) != 1) stop("Invalid Input: p must be a single integer")
  if(!is.numeric(p) | is.na(p) | p <= 0 | p > 1) stop("Invalid Input: p must be a single value (0,1]")
  
  if(length(s) != 1) stop("Invalid Input: s must be a single value")
  if(!is.numeric(s) | is.na(s) | s <= 0 | s > 1) stop("Invalid Input: s must be a single value (0,1]")
  
  if(length(m) != 1) stop("Invalid Input: m must be a single integer")
  if(!is.numeric(m) | is.na(m) | m <= 0) stop("Invalid Input: m must be an integer greater than 0")
  
  if(length(r) != 1) stop("Invalid Input: r must be a single integer")
  if(!is.numeric(r) | is.na(r) | r <= 0) stop("Invalid Input: r must be an integer greater than 0")
  
  if(length(a) != 1) stop("Invalid Input: a must be a single value")
  if(!is.numeric(a) | is.na(a) | a < 0 | a > 1) stop("Invalid Input: a must be a single value [0,1]")
  
  if(!(monitor %in% c(TRUE, FALSE))) stop("Invalid Input: monitor must be TRUE, FALSE, or plot")
}