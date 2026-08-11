#' Set or set the root data path
#' 
#' @export
#' @param path the data path
#' @param filename chr, the name of the file where the path is stored
#' @return the input path
set_root_data_path = function(path = "/mnt/s1/projects/ecocast/coredata/noaa/nhc/hurdat2",
                              filename = "~/.hurdat2"){
  
  cat(path, "\n", file = filename)
  path
}

#' @export
#' @rdname set_root_data_path
root_data_path = function(filename = "~/.hurdat2"){
  readLines(filename)
}


#' Retrieve the root data path, possibly adding segments
#' 
#' @export
#' @param ... chr, path segments and/or file names to append to the `path`
#' @param path chr, the root data path
#' @return path description
hurdat_path = function(..., path = root_data_path()){
  file.path(path, ...)
}
