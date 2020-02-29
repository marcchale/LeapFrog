distMatrixTest <- function(distMatrix){
if(!is.matrix(distMatrix)) stop("Invalid Input: distMatrix must be a matrix")
if(dim(distMatrix)[1] != dim(distMatrix)[2]) stop("Invalid Input: distMatrix must be a square matrix")
if(any(is.na(distMatrix))) stop("Invalid Input: distMatrix must not contain NAs")
if(!all(apply(distMatrix, c(1,2), function(x) is.numeric(x)))) stop("Invalid Input: distMatrix must contain all numeric inputs")
}

coordDFTest <- function(coordDF){
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
  return(coordDF)
}

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