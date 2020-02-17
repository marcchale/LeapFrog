LFStartupMessage <- function()
{
  # Startup message obtained as 
  msg <- c(paste0(
    " __    ____    __    ____    ____  ____  _____  ___ 
(  )  ( ___)  /__\\  (  _ \\  ( ___)(  _ \\(  _  )/ __)
 )(__  )__)  /(__)\\  )___/   )__)  )   / )(_)(( (_-.
(____)(____)(__)(__)(__)    (__)  (_)\\_)(_____)\\___/ ", packageVersion("LeapFrog")),
    "\nType 'citation(\"LeapFrog\")' for citing this R package in publications.")
  return(msg)
}

.onAttach <- function(lib, pkg)
{
  # startup message
  msg <- LFStartupMessage()
  if(!interactive())
    msg[1] <- paste("Package 'LF' version", packageVersion("LeapFrog"))
  packageStartupMessage(msg)
  invisible()
}

.onUnload <- function(libpath){
  library.dynam.unload("LeapFrog", libpath)
}

