# nocov start

.onLoad <- function(libname, pkgname){
  py_file = system.file("python", "python.py", package = "LeapFrog")
  reticulate::source_python(py_file)
}

.onUnload <- function(libpath){
  library.dynam.unload("LeapFrog", libpath)
}