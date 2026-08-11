#' List HURDAT2 data file(s)
#' 
#' @export
#' @param path chr the HURDAT2 data path
#' @return chr, vector of one or more HURDAT2 data file descriptions
list_hurdat = function(path = hurdat_path("raw")){
  list.files(path, full.names = TRUE, pattern = "^.*\\.txt$")
}

convert_longitude = function(x = c("94.8W", "95.4W", "96.0W", "96.5W", "96.8W", "97.0W")){
  n = nchar(x)
  west = substring(x, n) == "W"
  lon = substring(x, 1, n-1) |> as.numeric()
  lon[west] = -1 * lon[west]
  lon
}
convert_latitude = function(x = c("28.0N", "28.0N", "28.0N", "28.1N", "28.2N", "28.2N")){
  n = nchar(x)
  south = substring(x, n) == "S"
  lat = substring(x, 1, n-1) |> as.numeric()
  lat[south] = -1 * lat[south]
  lat
}

#' Read HURDAT2 data
#' 
#' @export
#' @seealso [AOML format description](https://www.aoml.noaa.gov/hrd/hurdat/hurdat2-format.pdf)
#' @param filename the name of the file
#' @return `read_hurdat_raw` returns a data frame while `read_hurdat` returns
#'   an sf POINT data frame
read_hurdat_raw = function(filename = list_hurdat() |> tail(n=1)){
  
  x = readLines(filename)
  N = length(x)
  header = which(nchar(x) < 40)
  start = header + 1
  end = c(header[-1] - 1, N)
  r = lapply(seq_along(header),
    function(i){
      h = x[header[i]] |>
        strsplit(",", fixed = TRUE)|>
        getElement(1) |>
        trimws()
      d = readr::read_csv(I(paste(x[start[i]:end[i]], collapse = "\n")),
                          col_types = readr::cols(date = readr::col_character(), 
                                                  "hhmm" = readr::col_character(), 
                                                  "record_id" = readr::col_character(),
                                                  "sys_status" = readr::col_character(),
                                                  "lat" = readr::col_character(),
                                                  "lon" = readr::col_character(),
                                                  "wind_max_sus" = readr::col_number(),
                                                  "min_pres" = readr::col_number(),
                                                  "wind_34kt_ne" = readr::col_number(),
                                                  "wind_34kt_se" = readr::col_number(),
                                                  "wind_34kt_sw" = readr::col_number(),
                                                  "wind_34kt_nw" = readr::col_number(),
                                                  "wind_50kt_ne" = readr::col_number(),
                                                  "wind_50kt_se" = readr::col_number(),
                                                  "wind_50kt_sw" = readr::col_number(),
                                                  "wind_50kt_nw" = readr::col_number(),
                                                  "wind_64kt_ne" = readr::col_number(),
                                                  "wind_64kt_se" = readr::col_number(),
                                                  "wind_64kt_sw" = readr::col_number(),
                                                  "wind_64kt_nw" = readr::col_number(),
                                                  "wind_max_radius" = readr::col_number()),
                                                  
                          col_names = c("date", 
                                        "hhmm", 
                                        "record_id",
                                        "sys_status",
                                        "lat",
                                        "lon",
                                        "wind_max_sus",
                                        "min_pres",
                                        "wind_34kt_ne",
                                        "wind_34kt_se",
                                        "wind_34kt_sw",
                                        "wind_34kt_nw",
                                        "wind_50kt_ne",
                                        "wind_50kt_se",
                                        "wind_50kt_sw",
                                        "wind_50kt_nw",
                                        "wind_64kt_ne",
                                        "wind_64kt_se",
                                        "wind_64kt_sw",
                                        "wind_64kt_nw",
                                        "wind_max_radius"),
                          skip = 0,
                          na = c("", "NA", "-999")) |>
        dplyr::mutate(id = h[1],
                      name = h[2],
                      datetime = as.POSIXct(paste(.data$date, .data$hhmm),
                                            format = "%Y%m%d %H%M",
                                            tz = "UTC"),
                      .before = 1) |>
        dplyr::select(-dplyr::any_of(c("date", "hhmm"))) |>
        dplyr::mutate(lon = convert_longitude(.data$lon),
                      lat = convert_latitude(.data$lat))
    }) |>
    dplyr::bind_rows()
  
  r
}


record_identifier = function(){
  c(C = "Closest approach to a coast, not followed by a landfall",
    G = "Genesis",
    I = "An intensity peak in terms of both pressure and wind",
    L = "Landfall (center of system crossing a coastline)",
    P = "Minimum in central pressure",
    R = "Provides additional detail on the intensity of the cyclone when rapid changes are underway",
    S = "Change of status of the system",
    T = "Provides additional detail on the track (position) of the cyclone",
    W = "Maximum sustained wind speed")
}

system_status = function(){
  c(TD = "Tropical cyclone of tropical depression intensity (< 34 knots)",
    TS = "Tropical cyclone of tropical storm intensity (34-63 knots)",
    HU = "Tropical cyclone of hurricane intensity (> 64 knots)",
    EX = "Extratropical cyclone (of any intensity)",
    SD = "Subtropical cyclone of subtropical depression intensity (< 34 knots)",
    SS = "Subtropical cyclone of subtropical storm intensity (> 34 knots)",
    LO = "A low that is neither a tropical cyclone, a subtropical cyclone, nor an extratropical cyclone (of any intensity)",
    WV = "Tropical Wave (of any intensity)")
}

#' @rdname read_hurdat_raw
#' @export
read_hurdat = function(filename = system.file("extdata/hurdat2-1851-2025-02272026.rds",
                                              package = "hurdat2")){
  readRDS(filename)
}


#' Read coastline from Natural Earth
#' 
#' Made with Natural Earth. Free vector and raster map data @ naturalearthdata.com.
#' 
#' @export
#' @param filename chr, the name of the file
#' @return sfc_MULTILINESTRING geometry
read_coast = function(filename = system.file("extdata/coastline.rds",
                                             package = "hurdat2")){
  readRDS(filename)
}