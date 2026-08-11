#' Retrieve the current HURDAT2 url
#' 
#' @export
#' @return chr, HURDAT2 url 
hurdat2_url = function(){
  "https://www.nhc.noaa.gov/data/hurdat/hurdat2-1851-2025-02272026.txt"
}

#' Fetch HURDAT2 data
#' 
#' @export
#' @param url chr, the URL of the source data
#' @param dest chr, the destination path
#' @return the output of [utils::download.file()], 0 for success
fetch_hurdat = function(url = hurdat2_url(),
                        dest = hurdat_path("raw")){
  dest = file.path(dest, basename(url))
  download.file(url, dest)
}
